{-# LANGUAGE BinaryLiterals #-}
module Bench where

import Prelude hiding (writeFile)
import Data.Binary (decodeFile)
import Data.Bitstream
import Data.ToBits
import Data.Word (Word8)
import Data.Bits (FiniteBits, testBit, setBit, zeroBits)

import qualified Data.ByteString as B

import           Criterion.Main


-- | flip a bytes bit order 0b00000001 -> 0b10000000
flipByte :: Word8 -> Word8
flipByte b = foldr f zeroBits [0..7]
  where f :: Int -> Word8 -> Word8
        f i w | testBit w (7-i) = setBit w i
              | otherwise       = w

flipByte2 :: Word8 -> Word8
flipByte2 0b00000000 = 0b00000000
flipByte2 0b10000000 = 0b00000001
flipByte2 0b01000000 = 0b00000010
flipByte2 0b11000000 = 0b00000011
flipByte2 0b00100000 = 0b00000100
flipByte2 0b10100000 = 0b00000101
flipByte2 0b01100000 = 0b00000110
flipByte2 0b11100000 = 0b00000111
flipByte2 0b00010000 = 0b00001000
flipByte2 0b10010000 = 0b00001001
flipByte2 0b01010000 = 0b00001010
flipByte2 0b11010000 = 0b00001011
flipByte2 0b00110000 = 0b00001100
flipByte2 0b10110000 = 0b00001101
flipByte2 0b01110000 = 0b00001110
flipByte2 0b11110000 = 0b00001111
flipByte2 0b00001000 = 0b00010000
flipByte2 0b10001000 = 0b00010001
flipByte2 0b01001000 = 0b00010010
flipByte2 0b11001000 = 0b00010011
flipByte2 0b00101000 = 0b00010100
flipByte2 0b10101000 = 0b00010101
flipByte2 0b01101000 = 0b00010110
flipByte2 0b11101000 = 0b00010111
flipByte2 0b00011000 = 0b00011000
flipByte2 0b10011000 = 0b00011001
flipByte2 0b01011000 = 0b00011010
flipByte2 0b11011000 = 0b00011011
flipByte2 0b00111000 = 0b00011100
flipByte2 0b10111000 = 0b00011101
flipByte2 0b01111000 = 0b00011110
flipByte2 0b11111000 = 0b00011111
flipByte2 0b00000100 = 0b00100000
flipByte2 0b10000100 = 0b00100001
flipByte2 0b01000100 = 0b00100010
flipByte2 0b11000100 = 0b00100011
flipByte2 0b00100100 = 0b00100100
flipByte2 0b10100100 = 0b00100101
flipByte2 0b01100100 = 0b00100110
flipByte2 0b11100100 = 0b00100111
flipByte2 0b00010100 = 0b00101000
flipByte2 0b10010100 = 0b00101001
flipByte2 0b01010100 = 0b00101010
flipByte2 0b11010100 = 0b00101011
flipByte2 0b00110100 = 0b00101100
flipByte2 0b10110100 = 0b00101101
flipByte2 0b01110100 = 0b00101110
flipByte2 0b11110100 = 0b00101111
flipByte2 0b00001100 = 0b00110000
flipByte2 0b10001100 = 0b00110001
flipByte2 0b01001100 = 0b00110010
flipByte2 0b11001100 = 0b00110011
flipByte2 0b00101100 = 0b00110100
flipByte2 0b10101100 = 0b00110101
flipByte2 0b01101100 = 0b00110110
flipByte2 0b11101100 = 0b00110111
flipByte2 0b00011100 = 0b00111000
flipByte2 0b10011100 = 0b00111001
flipByte2 0b01011100 = 0b00111010
flipByte2 0b11011100 = 0b00111011
flipByte2 0b00111100 = 0b00111100
flipByte2 0b10111100 = 0b00111101
flipByte2 0b01111100 = 0b00111110
flipByte2 0b11111100 = 0b00111111
flipByte2 0b00000010 = 0b01000000
flipByte2 0b10000010 = 0b01000001
flipByte2 0b01000010 = 0b01000010
flipByte2 0b11000010 = 0b01000011
flipByte2 0b00100010 = 0b01000100
flipByte2 0b10100010 = 0b01000101
flipByte2 0b01100010 = 0b01000110
flipByte2 0b11100010 = 0b01000111
flipByte2 0b00010010 = 0b01001000
flipByte2 0b10010010 = 0b01001001
flipByte2 0b01010010 = 0b01001010
flipByte2 0b11010010 = 0b01001011
flipByte2 0b00110010 = 0b01001100
flipByte2 0b10110010 = 0b01001101
flipByte2 0b01110010 = 0b01001110
flipByte2 0b11110010 = 0b01001111
flipByte2 0b00001010 = 0b01010000
flipByte2 0b10001010 = 0b01010001
flipByte2 0b01001010 = 0b01010010
flipByte2 0b11001010 = 0b01010011
flipByte2 0b00101010 = 0b01010100
flipByte2 0b10101010 = 0b01010101
flipByte2 0b01101010 = 0b01010110
flipByte2 0b11101010 = 0b01010111
flipByte2 0b00011010 = 0b01011000
flipByte2 0b10011010 = 0b01011001
flipByte2 0b01011010 = 0b01011010
flipByte2 0b11011010 = 0b01011011
flipByte2 0b00111010 = 0b01011100
flipByte2 0b10111010 = 0b01011101
flipByte2 0b01111010 = 0b01011110
flipByte2 0b11111010 = 0b01011111
flipByte2 0b00000110 = 0b01100000
flipByte2 0b10000110 = 0b01100001
flipByte2 0b01000110 = 0b01100010
flipByte2 0b11000110 = 0b01100011
flipByte2 0b00100110 = 0b01100100
flipByte2 0b10100110 = 0b01100101
flipByte2 0b01100110 = 0b01100110
flipByte2 0b11100110 = 0b01100111
flipByte2 0b00010110 = 0b01101000
flipByte2 0b10010110 = 0b01101001
flipByte2 0b01010110 = 0b01101010
flipByte2 0b11010110 = 0b01101011
flipByte2 0b00110110 = 0b01101100
flipByte2 0b10110110 = 0b01101101
flipByte2 0b01110110 = 0b01101110
flipByte2 0b11110110 = 0b01101111
flipByte2 0b00001110 = 0b01110000
flipByte2 0b10001110 = 0b01110001
flipByte2 0b01001110 = 0b01110010
flipByte2 0b11001110 = 0b01110011
flipByte2 0b00101110 = 0b01110100
flipByte2 0b10101110 = 0b01110101
flipByte2 0b01101110 = 0b01110110
flipByte2 0b11101110 = 0b01110111
flipByte2 0b00011110 = 0b01111000
flipByte2 0b10011110 = 0b01111001
flipByte2 0b01011110 = 0b01111010
flipByte2 0b11011110 = 0b01111011
flipByte2 0b00111110 = 0b01111100
flipByte2 0b10111110 = 0b01111101
flipByte2 0b01111110 = 0b01111110
flipByte2 0b11111110 = 0b01111111
flipByte2 0b00000001 = 0b10000000
flipByte2 0b10000001 = 0b10000001
flipByte2 0b01000001 = 0b10000010
flipByte2 0b11000001 = 0b10000011
flipByte2 0b00100001 = 0b10000100
flipByte2 0b10100001 = 0b10000101
flipByte2 0b01100001 = 0b10000110
flipByte2 0b11100001 = 0b10000111
flipByte2 0b00010001 = 0b10001000
flipByte2 0b10010001 = 0b10001001
flipByte2 0b01010001 = 0b10001010
flipByte2 0b11010001 = 0b10001011
flipByte2 0b00110001 = 0b10001100
flipByte2 0b10110001 = 0b10001101
flipByte2 0b01110001 = 0b10001110
flipByte2 0b11110001 = 0b10001111
flipByte2 0b00001001 = 0b10010000
flipByte2 0b10001001 = 0b10010001
flipByte2 0b01001001 = 0b10010010
flipByte2 0b11001001 = 0b10010011
flipByte2 0b00101001 = 0b10010100
flipByte2 0b10101001 = 0b10010101
flipByte2 0b01101001 = 0b10010110
flipByte2 0b11101001 = 0b10010111
flipByte2 0b00011001 = 0b10011000
flipByte2 0b10011001 = 0b10011001
flipByte2 0b01011001 = 0b10011010
flipByte2 0b11011001 = 0b10011011
flipByte2 0b00111001 = 0b10011100
flipByte2 0b10111001 = 0b10011101
flipByte2 0b01111001 = 0b10011110
flipByte2 0b11111001 = 0b10011111
flipByte2 0b00000101 = 0b10100000
flipByte2 0b10000101 = 0b10100001
flipByte2 0b01000101 = 0b10100010
flipByte2 0b11000101 = 0b10100011
flipByte2 0b00100101 = 0b10100100
flipByte2 0b10100101 = 0b10100101
flipByte2 0b01100101 = 0b10100110
flipByte2 0b11100101 = 0b10100111
flipByte2 0b00010101 = 0b10101000
flipByte2 0b10010101 = 0b10101001
flipByte2 0b01010101 = 0b10101010
flipByte2 0b11010101 = 0b10101011
flipByte2 0b00110101 = 0b10101100
flipByte2 0b10110101 = 0b10101101
flipByte2 0b01110101 = 0b10101110
flipByte2 0b11110101 = 0b10101111
flipByte2 0b00001101 = 0b10110000
flipByte2 0b10001101 = 0b10110001
flipByte2 0b01001101 = 0b10110010
flipByte2 0b11001101 = 0b10110011
flipByte2 0b00101101 = 0b10110100
flipByte2 0b10101101 = 0b10110101
flipByte2 0b01101101 = 0b10110110
flipByte2 0b11101101 = 0b10110111
flipByte2 0b00011101 = 0b10111000
flipByte2 0b10011101 = 0b10111001
flipByte2 0b01011101 = 0b10111010
flipByte2 0b11011101 = 0b10111011
flipByte2 0b00111101 = 0b10111100
flipByte2 0b10111101 = 0b10111101
flipByte2 0b01111101 = 0b10111110
flipByte2 0b11111101 = 0b10111111
flipByte2 0b00000011 = 0b11000000
flipByte2 0b10000011 = 0b11000001
flipByte2 0b01000011 = 0b11000010
flipByte2 0b11000011 = 0b11000011
flipByte2 0b00100011 = 0b11000100
flipByte2 0b10100011 = 0b11000101
flipByte2 0b01100011 = 0b11000110
flipByte2 0b11100011 = 0b11000111
flipByte2 0b00010011 = 0b11001000
flipByte2 0b10010011 = 0b11001001
flipByte2 0b01010011 = 0b11001010
flipByte2 0b11010011 = 0b11001011
flipByte2 0b00110011 = 0b11001100
flipByte2 0b10110011 = 0b11001101
flipByte2 0b01110011 = 0b11001110
flipByte2 0b11110011 = 0b11001111
flipByte2 0b00001011 = 0b11010000
flipByte2 0b10001011 = 0b11010001
flipByte2 0b01001011 = 0b11010010
flipByte2 0b11001011 = 0b11010011
flipByte2 0b00101011 = 0b11010100
flipByte2 0b10101011 = 0b11010101
flipByte2 0b01101011 = 0b11010110
flipByte2 0b11101011 = 0b11010111
flipByte2 0b00011011 = 0b11011000
flipByte2 0b10011011 = 0b11011001
flipByte2 0b01011011 = 0b11011010
flipByte2 0b11011011 = 0b11011011
flipByte2 0b00111011 = 0b11011100
flipByte2 0b10111011 = 0b11011101
flipByte2 0b01111011 = 0b11011110
flipByte2 0b11111011 = 0b11011111
flipByte2 0b00000111 = 0b11100000
flipByte2 0b10000111 = 0b11100001
flipByte2 0b01000111 = 0b11100010
flipByte2 0b11000111 = 0b11100011
flipByte2 0b00100111 = 0b11100100
flipByte2 0b10100111 = 0b11100101
flipByte2 0b01100111 = 0b11100110
flipByte2 0b11100111 = 0b11100111
flipByte2 0b00010111 = 0b11101000
flipByte2 0b10010111 = 0b11101001
flipByte2 0b01010111 = 0b11101010
flipByte2 0b11010111 = 0b11101011
flipByte2 0b00110111 = 0b11101100
flipByte2 0b10110111 = 0b11101101
flipByte2 0b01110111 = 0b11101110
flipByte2 0b11110111 = 0b11101111
flipByte2 0b00001111 = 0b11110000
flipByte2 0b10001111 = 0b11110001
flipByte2 0b01001111 = 0b11110010
flipByte2 0b11001111 = 0b11110011
flipByte2 0b00101111 = 0b11110100
flipByte2 0b10101111 = 0b11110101
flipByte2 0b01101111 = 0b11110110
flipByte2 0b11101111 = 0b11110111
flipByte2 0b00011111 = 0b11111000
flipByte2 0b10011111 = 0b11111001
flipByte2 0b01011111 = 0b11111010
flipByte2 0b11011111 = 0b11111011
flipByte2 0b00111111 = 0b11111100
flipByte2 0b10111111 = 0b11111101
flipByte2 0b01111111 = 0b11111110
flipByte2 0b11111111 = 0b11111111

--------------------------------------------------------------------------------
-- Reading a serialized module.
readModule :: FilePath -> IO [BitCode]
readModule = decodeFile

writeModule :: FilePath -> [BitCode] -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel

--------------------------------------------------------------------------------
-- Turing a stream of Bits into Bytes

-- bench helper
repBs :: Int -> Bitstream ()
repBs n = sequence_ [sequence_ (replicate n (emitBit True)) | n <- [1..n]]

flipByteBs :: Int -> B.ByteString
flipByteBs n = B.map flipByte . B.pack $ [ fromIntegral i | i <- [0..n]]

flipByte2Bs :: Int -> B.ByteString
flipByte2Bs n = B.map flipByte2 . B.pack $ [ fromIntegral i | i <- [0..n]]

main :: IO ()
main = do
  bc <- readModule "bench/data/Main.bcbin"
  defaultMain
    [ bgroup "synthetic"
      -- this will produce quadratic data.
      -- 1 11 111 1111 1111'1 1111'11 1111'111 1111'1111 ...
      [ bench (show i) $ nf (execBitstream 0 . repBs) i | i <- [50,100..200] ]
    , bgroup "real world"
      [ bench "Main" $ nfIO (writeModule "Main-bench.bc" bc)
      --  , bench "HelloWorld2" $ nfIO (writeModule "HelloWorld2.bc" =<< readModule "bench/data/HelloWorld2.mod")
      ]
    , bgroup "flip bytes"
      [ bench (show ((2^i)/1024/1024) ++ "M") $ nf flipByteBs (2^i) | i <- [19..25]] -- 5k to 32M
    , bgroup "flip bytes2"
      [ bench (show ((2^i)/1024/1024) ++ "M") $ nf flipByte2Bs (2^i) | i <- [19..25]]
    ]