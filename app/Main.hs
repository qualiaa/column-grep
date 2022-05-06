{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Monad(forM_)
import Data.Bitraversable (bimapM)
import Data.Either
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashSet as S

import ParseArguments
import Result
import qualified ColSpec
import qualified Regex as RE

fieldSeparator = ','
recordSeparators = "\n\r"

main :: IO ()
main = do
  args <- getArgs
  (firstLine, remainingLines) <- LC8.span (`notElem` recordSeparators) <$> LC8.getContents
  let parseResults = parseArguments args

  if any isLeft parseResults then
    forM_ (zip args parseResults) (\case
        (arg, Left err) -> putStrLn $ "Error parsing argument `" ++ arg ++ "': " ++ err
        _ -> return ())

  else do
    let columnNames = C8.split fieldSeparator $ LC8.toStrict firstLine
        numColumns = length columnNames

        resolveColspecs = fmap allSucceed . mapM (ColSpec.resolve columnNames (numColumns - 1))
        resolveComparators = fmap bothSucceed . bimapM resolveColspecs RE.compile

        (outputColumns, comparators) = partitionEithers $ rights parseResults

    resolvedOutputs <- resolveColspecs $ concat outputColumns
    resolvedComparators <- sequence <$> mapM resolveComparators comparators :: IO (Result [([Int], RE.Regex)])

    case resolvedOutputs >> resolvedComparators of
       Left err -> print err
       otherwise -> process (S.fromList resolvedOutputs) resolvedComparators remainingLines
    print resolvedOutputs

--process :: S.HashSet Int -> [([Int], RE.Regex)] -> LC8.ByteString -> IO ()
--process outputIndices comparators input =
--  forM_ (LC8.splitWith (`elem` recordSeparators) input) (\line -> do
--    let lineS = LC8.toStrict line
--        fields = C8.split fieldSeparator lineS
--                                                                                                )
