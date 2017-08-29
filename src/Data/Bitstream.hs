{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, KindSignatures, BinaryLiterals #-}
module Data.Bitstream where

import Prelude hiding (last, words)

import Data.Word
import Data.Bits

import qualified Data.List as L

import Data.Monoid ((<>))
import Control.Applicative (liftA2)
import Control.Monad.Fix

-- | The position in the stream. 
type Position = Int

-- | A @Word8@ buffer, tracking the number of bits.
newtype Buff = Buff (Int, Word8) deriving (Eq, Ord)

nullBuff :: Buff
nullBuff = Buff (0,0)

-- | Adding two buffers. E.g. copying over the bits from one buffer into the
-- other. If we complete a @Word8@, this will be in the first position of the
-- result.
--
-- The bitcode stream will look like
--
-- 0      8 9     16 17    32
-- |      | |      | |      |
-- 10101111 00001100 10101010
--
-- The on disk representation will however look like
--
-- 11110101 00110000 01010101
-- |      | |      | |      |
-- 8      0 16     9 32    17
--
-- Thus, to now have to rearange the bits before writing to disk, we will
-- encode them in disk order (as opposed to stream order) from the start.
--
-- Assume we have two buffers (1, 0b00000001) and (1, 0b00000001), we expect
-- to obtain (2, 0b00000011). Thus we want to shift the second buffer by the
-- length of the first, and or them.
--
-- In the spill case (5, 0b00010101) and (4, 0b00001111) we expect to get
-- (Just 0b11110101, (1, b000000001)) 
--
addBuff :: Buff -> Buff -> (Maybe Word8, Buff)
addBuff (Buff (n,w)) (Buff (n',w')) | n+n' < 8  = (Nothing
                                                  , Buff (n+n', w .|. (shift w' n)))
                                    | otherwise = (Just (w .|. (shift w' n))
                                                  , Buff ((n+n') `mod` 8, shift w' (n-8)))

-- | A stream is a number of Words, a buffer and a position (length of the stream) marker.
data Stream f a = S
  { words  :: f Word8
  , buffer :: Buff
  , pos    :: Position
  }

deriving instance Eq (f Word8) => Eq (Stream f a)
deriving instance Ord (f Word8) => Ord (Stream f a)

class Last (f :: * -> *) where last :: f a -> a
instance Last [] where last = L.last

instance ( Monoid (f Word8)
         , Foldable f
         , Applicative f) => Monoid (Stream f a) where
  mempty = S mempty nullBuff 0
  (S w b p) `mappend` (S w' b' p') = case b of
    -- there are no bits in the buffer. We can simply
    -- concatinate lhs and rhs
    Buff (0,_) -> S (w <> w') b' (p+p')
    -- there are already @n@ bites in the buffer. We will
    -- need to shift all the bits in the RHS left by 8-n.
    Buff (n,c) | null w' -> case addBuff b b' of
                               (Just w'', b'') -> S (w <> pure w'') b'' (p+p')
                               (Nothing,  b'') -> S w b'' (p+p')
               | otherwise -> let (w'', l) = foldl (go n) (mempty, c) w'
                              in case addBuff (Buff (n, l)) b' of
                                   (Just w''', b'') -> S (w <> w'' <> pure w''') b'' (p+p')
                                   (Nothing,   b'') -> S (w <> w'')              b'' (p+p')
      where go :: (Monoid (t Word8), Applicative t, Foldable t)
               => Int -> (t Word8, Word8) -> Word8 -> (t Word8, Word8)
            go n (acc, b) b' = (acc <> pure (b .|. shift b' n), shift b' (n-8))

-- mappend is not cheap here.
type ListStream = Stream [] Word8

newtype Bitstream a = Bitstream { unBitstream :: Position -> (ListStream, a) }

runBitstream :: Position -> Bitstream a -> (ListStream, a)
runBitstream p (Bitstream f) = f p
execBitstream :: Position -> Bitstream a -> [Word8]
execBitstream p a = words . fst . runBitstream p $ a >> alignWord8
evalBitstream :: Position -> Bitstream a -> a
evalBitstream p = snd . runBitstream p

-- * Functor
instance Functor Bitstream where
  fmap f x = Bitstream $ \pos -> let
    (s, x') = unBitstream x pos
    in (s, f x')

-- * Applicative
instance Applicative Bitstream where
  pure r = Bitstream $ \pos -> (S mempty nullBuff pos, r)
  x <*> y = Bitstream $ \pos -> let
    ((S w b pos'), f) = unBitstream x pos
    ((S w' b' pos''), a) = unBitstream y pos'
    in ((S w b pos') <> (S w' b' (pos''-pos')), f a)

-- * Monad
instance Monad Bitstream where
  return r = Bitstream $ \pos -> (S mempty nullBuff pos, r)
  x >>= y = Bitstream $ \pos -> let
    ((S w b pos'), res) = unBitstream x pos
    ((S w' b' pos''), res') = unBitstream (y res) pos'
    in ((S w b pos') <> (S w' b' (pos''-pos')), res')

-- * MonadFix
instance MonadFix Bitstream where
  mfix f = Bitstream $ \pos -> let
    (s, r) = unBitstream (f r) pos
    in (s, r)

-- Monadic Bitstream API
loc :: Bitstream Position
loc = Bitstream $ \pos -> (S mempty nullBuff pos, pos)

emitBit :: Bool -> Bitstream ()
emitBit True  = Bitstream $ \pos -> (S mempty (Buff (1, 0b00000001)) 1, ())
emitBit False = Bitstream $ \pos -> (S mempty (Buff (1, 0b00000000)) 1, ())

emitBits :: Int -> Word8 -> Bitstream ()
emitBits n b = Bitstream $ \pos -> (S mempty (Buff (n, b)) n, ())

alignWord8 :: Bitstream ()
alignWord8 = loc >>= \x -> emitBits (8 - (x `mod` 8)) 0

-- Show instances. These make parsing debug output much easier.

showWord8 :: Word8 -> String
showWord8 w = '0':'b':(map f $ reverse [testBit w i | i <- [0..7]])
  where f True  = '1'
        f False = '0'

instance Show Buff where
  show (Buff (n, w)) = show n ++ " bits: " ++ showWord8 w

instance (Functor f, Foldable f) => Show (Stream f a) where
  show (S ws (Buff (n,b)) p) = show p ++ " bits: " ++ foldl1 (\x y -> x ++ " " ++ y) (fmap showWord8' ws) ++ " " ++ take n (showWord8' b)
    where showWord8' w = map f $ reverse [testBit w i | i <- [0..7]]
          f True = '1'
          f False = '0'
