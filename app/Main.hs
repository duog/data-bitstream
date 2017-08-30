-- Test application for profiling
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fprof-auto #-}
module Main where

import System.Environment (getArgs)
import System.FilePath ((-<.>))
import Prelude hiding (writeFile)
import Data.Binary (decodeFile)
import Data.Bitstream
import Data.ToBits

--------------------------------------------------------------------------------
-- Reading a serialized module.
readModule :: FilePath -> IO [BitCode]
readModule = decodeFile

writeModule :: FilePath -> [BitCode] -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn $ "please provide an input file"
  (f:_) -> do
    bc <- {-# SCC "reading" #-} readModule f
    {-# SCC "writing" #-} writeModule (f -<.> "bc") bc
    
