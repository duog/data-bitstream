{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, KindSignatures, BinaryLiterals, RecursiveDo, LambdaCase #-}
module Data.Bitstream where

import Prelude hiding (last, words, writeFile, tail)

import Data.Word
import Data.Bits

import qualified Data.List as L
import qualified Data.ByteString as B

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

bytes :: Bitstream Word32
bytes = Bitstream $ \pos -> (S mempty nullBuff pos, fromIntegral $ pos `div` 8)

emitBit :: Bool -> Bitstream ()
emitBit True  = Bitstream $ \pos -> (S mempty (Buff (1, 0b00000001)) 1, ())
emitBit False = Bitstream $ \pos -> (S mempty (Buff (1, 0b00000000)) 1, ())

emitBits :: Int -> Word8 -> Bitstream ()
emitBits n b = Bitstream $ \pos -> (S mempty (Buff (n, b)) n, ())

emitWord8 :: Word8 -> Bitstream ()
emitWord8 w = Bitstream $ \pos -> (S [w] nullBuff 8, ())

emitWord32R :: Word32 -> Bitstream ()
emitWord32R w = Bitstream $ \pos -> (S [fromIntegral (shift w (-24))
                                       ,fromIntegral (shift w (-16))
                                       ,fromIntegral (shift w  (-8))
                                       ,fromIntegral w] nullBuff 32, ())
emitWord32 :: Word32 -> Bitstream ()
emitWord32 w = Bitstream $ \pos -> (S [fromIntegral (shift w  (-0))
                                      ,fromIntegral (shift w  (-8))
                                      ,fromIntegral (shift w (-16))
                                      ,fromIntegral (shift w (-24))] nullBuff 32, ())

emitFixed :: Int -> Word64 -> Bitstream ()
emitFixed n w | n < 8  = Bitstream $ \pos -> (S mempty    (Buff (n, off 0 w)) n, ())
              | n < 16 = Bitstream $ \pos -> (S [off  0 w] (Buff (n-8, off 8 w)) n, ())
              | n < 24 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w] (Buff (n-16, off 16 w)) n, ())
              | n < 32 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w
                                                ,off 16 w] (Buff (n-24, off 24 w)) n, ())
              | n < 40 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w
                                                ,off 16 w
                                                ,off 24 w] (Buff (n-32, off 32 w)) n, ())
              | n < 48 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w
                                                ,off 16 w
                                                ,off 24 w
                                                ,off 32 w] (Buff (n-40, off 40 w)) n, ())
              | n < 56 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w
                                                ,off 16 w
                                                ,off 24 w
                                                ,off 32 w
                                                ,off 40 w] (Buff (n-48, off 48 w)) n, ())
              | n < 64 = Bitstream $ \pos -> (S [off  0 w
                                                ,off  8 w
                                                ,off 16 w
                                                ,off 24 w
                                                ,off 32 w
                                                ,off 40 w
                                                ,off 48 w] (Buff (n-56, off 56 w)) n, ())
              | n == 64 = Bitstream $ \pos -> (S [ off  0 w, off  8 w, off 16 w, off 24 w
                                                 , off 32 w, off 40 w, off 48 w, off 56 w]
                                               nullBuff
                                               64, ())
              | otherwise = error $ "invalid number of bits. Cannot emit " ++ show n ++ " bits from Word64."
    where off :: Int -> Word64 -> Word8
          off n w = fromIntegral (shift w (-n))

emitVBR :: Int -> Word64 -> Bitstream ()
emitVBR n w = do
  emitFixed (n-1) w
  let tail = shift w (-n+1)
    in if popCount tail == 0
       then emitBit False
       else emitBit True >> emitVBR n tail

emitChar6 :: Char -> Bitstream ()
emitChar6 '_' = emitBits 6 63
emitChar6 '.' = emitBits 6 62
emitChar6 c | 'a' <= c && c <= 'z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'a')
            | 'A' <= c && c <= 'Z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'A') + 26
            | '0' <= c && c <= '9' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum '0') + 52
            | otherwise = fail $ "char '" ++ c:"' not in [a-zA-Z0-9._]"

alignWord8 :: Bitstream ()
alignWord8 = flip mod 8 <$> loc >>= \case
  0 -> pure ()
  x -> emitBits (8 - x) 0

alignWord32 :: Bitstream ()
alignWord32 = flip mod 32 <$> loc >>= \case
  0 -> pure ()
  x -> emitBits (32 - x) 0

writeFile :: FilePath -> Bitstream a -> IO ()
writeFile f = B.writeFile f . B.pack . execBitstream 0 


-- * BitCode Header
-- | put the BitCodeHeader, on darwin a special wrapper is
-- apparently only required, to make it compatible with
-- the system archiver.
withHeader
  :: Bool    -- ^ wrap in darwin header
  -> Bitstream () -- ^ body bitcode
  -> Bitstream ()
withHeader isDarwin body = mdo
  -- if it's darwin, we add the header with the length of the body
  -- (#words * 4 bytes) as well as the LLVM IR Header (4 bytes)
  if isDarwin
    then emitDarwinHeader b
    else pure ()
  body'
  b <- bytes
  return ()
  where body' = emitLLVMIRHeader >> body >> alignWord32
        emitDarwinHeader
          :: Word32 -- ^ number of bytes in body
          -> Bitstream ()
        emitDarwinHeader len = do
          emitWord32R 0x0b17c0de                -- 0x0b17c0de   4
          emitWord32  0                         -- version: 0  +4
          emitWord32  20                        -- offset: 20  +4 <--.
          emitWord32  len                       -- length      +4    |
          emitWord32  cpuType                   --             +4 => 20 in total.
            where
              -- We are hardcoding x86_64 for now.
              cpuType :: Word32
              cpuType = 0x01000000 -- DARWIN_CPU_ARCH_ABI64
                      +          7 -- DARWIN_CPU_TYPE_X86(7),
                                   -- DARWIN_CPU_TYPE_ARM(12),
                                   -- DARWIN_CPU_TYPE_POWERPC(18)
        emitLLVMIRHeader :: Bitstream ()
        emitLLVMIRHeader = emitWord32R 0x4243c0de -- 'BC' 0xc0de

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

