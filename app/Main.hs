{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Monad(forM_)
import Data.Bitraversable (bimapM)
import Data.List (foldl', sortBy)
import Data.Either
import System.Environment (getArgs)
import Text.Regex.Base (matchTest)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

        (outputColumnSpecs, comparatorSpecs) = partitionEithers $ rights parseResults

    resolvedOutputColumns <- resolveColspecs $ concat outputColumnSpecs
    resolvedComparators <- sequence <$> mapM resolveComparators comparatorSpecs :: IO (Result [([Int], RE.Regex)])

    case (do
      outputColumns <- S.fromList <$> resolvedOutputColumns
      comparators <- comparatorsByColumn <$> resolvedComparators
      let inputLines = LC8.splitWith (`elem` recordSeparators) remainingLines
      return $ processRecords outputColumns comparators inputLines
      ) of Left err -> putStrLn err
           Right lines -> mapM_ LC8.putStrLn lines

comparatorsByColumn :: [([Int], RE.Regex)] -> M.Map Int [RE.Regex]
comparatorsByColumn = foldl' addToMap M.empty
  where addToMap m (ix, re) = foldl' (\m i -> M.insertWith (++) i [re] m) m ix

processRecords :: S.Set Int -> M.Map Int [RE.Regex]  -> [LC8.ByteString] -> [LC8.ByteString]
processRecords outputIndices comparators = filter recordMatches

  where recordMatches = or . map fieldMatches . toFields

        toFields = zip [1..] . map LC8.toStrict . LC8.split fieldSeparator

        fieldMatches :: (Int, C8.ByteString) -> Bool
        fieldMatches (i, field) = let regexes = M.findWithDefault [] i comparators
                                  in or $ matchTest <$> regexes <*> pure field
