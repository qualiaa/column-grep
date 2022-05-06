module Main where

import Data.Either
import System.Environment (getArgs)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LC8
import ParseArguments

main :: IO ()
main = do
  args <- getArgs
  (firstLine, remainingLines) <- LC8.span (`notElem` recordSeparators) <$> LC8.getContents
  let parseResults = parseArguments args

  if any isLeft parseResults then
    forM_ (zip args parseResults) (\case
        (arg, Left err) -> putStrLn $ "Error parsing argument `" ++ arg ++ "': " ++ err
        _ -> return ())

