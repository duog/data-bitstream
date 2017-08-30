module Bench where

import Prelude hiding (writeFile)
import Data.Binary (decodeFile)
import Data.Bitstream
import Data.ToBits
import Data.Word (Word8)
import Data.Bits (FiniteBits, setBit, zeroBits)

import           Criterion.Main

--------------------------------------------------------------------------------
-- Reading a serialized module.
readModule :: FilePath -> IO [BitCode]
readModule = decodeFile

writeModule :: FilePath -> [BitCode] -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel

--------------------------------------------------------------------------------
-- Turing a stream of Bits into Bytes

main :: IO ()
main = do
  bc <- readModule "bench/data/Main.bcbin"
  defaultMain [
    bgroup "bistream"
      [ bench "HelloWorld" $ nfIO (writeModule "HelloWorld.bc" bc)
      --  , bench "HelloWorld2" $ nfIO (writeModule "HelloWorld2.bc" =<< readModule "bench/data/HelloWorld2.mod")
      ]
    ]
