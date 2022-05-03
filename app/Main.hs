module Main where

import System.Environment (getArgs)
import Lib
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 (pack)
import ParseArguments

main :: IO ()
main = do
  args <- fmap (map pack) getArgs
  print $ map (parseOnly parseArgument) args

