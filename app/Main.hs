{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Monad(forM_)
import Data.Bitraversable (bimapM)
import Data.List (foldl')
import Data.Either
import Data.Profunctor (dimap)
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
outputRecordSeparator = '\n'

type Field = C8.ByteString
type Record = [Field]
type FieldIndex = Int
type Comparators = M.Map FieldIndex [RE.Regex]

main :: IO ()
main = do
  args <- getArgs
  let parseResults = parseArguments args

  if any isLeft parseResults then
    forM_ (zip args parseResults) (\case
        (arg, Left err) -> putStrLn $ "Error parsing argument `" ++ arg ++ "': " ++ err
        _ -> return ())

  else uncurry process . partitionEithers $ rights parseResults

process outputColumnSpecs  comparatorSpecs = do
    (firstLine, remainingLines) <- LC8.span (`notElem` recordSeparators) <$> LC8.getContents

    let columnNames = toFields firstLine
        numColumns = length columnNames

        resolveColspecs = fmap allSucceed . mapM (ColSpec.resolve columnNames numColumns)
        resolveComparator = fmap bothSucceed . bimapM resolveColspecs RE.compile

    resolvedOutputColumns <- resolveColspecs $ concat outputColumnSpecs
    resolvedComparators <- sequence <$> mapM resolveComparator comparatorSpecs :: IO (Result [([FieldIndex], RE.Regex)])

    case (do
      outputColumns <- resolvedOutputColumns
      comparators <- resolvedComparators

      let outputColumns' = S.fromList outputColumns
          filterFields' = map (filterFields outputColumns')
          filterRecords' = filterRecords $ comparatorsByColumn comparators

      processRecords <- case (outputColumns, comparators) of
        ([], []) -> Left "Must specify at least one column-spec or comparator"
        (_, [])  -> return filterFields'
        ([], _)  -> return filterRecords'
        (_, _)   -> return $ filterFields' . filterRecords'

      let newHeader = if null outputColumns
            then firstLine
            else fromFields $ filterFields outputColumns' columnNames
          newBody = processRecords `withRecords` remainingLines

      return $ LC8.append (newHeader `LC8.snoc` outputRecordSeparator) newBody

      ) of Left err -> putStrLn err
           Right lines -> LC8.putStrLn lines

comparatorsByColumn :: [([FieldIndex], RE.Regex)] -> Comparators
comparatorsByColumn = foldl' addToMap M.empty
  where addToMap m (ix, re) = foldl' (\m i -> M.insertWith (++) i [re] m) m ix

filterRecords :: Comparators  -> [Record] -> [Record]
filterRecords comparators = filter keep
  where keep = or . map fieldMatches . zip [1..]

        fieldMatches :: (FieldIndex, Field) -> Bool
        fieldMatches (fieldIndex, field) =
          let regexes = M.findWithDefault [] fieldIndex comparators in
            or $ matchTest <$> regexes <*> pure field

-- TODO: Add options to a) allow duplication and b) respect colspec ordering
filterFields :: S.Set FieldIndex -> [Field] -> [Field]
filterFields targetIndices = withIndices $ filter keep

  where keep (fieldIndex, field) = fieldIndex `S.member` targetIndices

toRecords :: LC8.ByteString -> [Record]
toRecords string =
  let recordStrings = LC8.splitWith (`elem` recordSeparators) string in
    map toFields recordStrings

toFields :: LC8.ByteString -> [Field]
toFields = C8.split fieldSeparator . LC8.toStrict

fromRecords :: [Record] -> LC8.ByteString
fromRecords records =
  let lines = map fromFields records in
    LC8.intercalate (LC8.singleton outputRecordSeparator) lines

fromFields :: [Field] -> LC8.ByteString
fromFields = LC8.fromStrict . C8.intercalate (C8.singleton fieldSeparator)

withRecords :: ([Record] -> [Record]) -> LC8.ByteString -> LC8.ByteString
withRecords = dimap toRecords fromRecords

withIndices :: (Integral i) => ([(i, a)] -> [(i, b)]) -> [a] -> [b]
withIndices = dimap (zip [1..]) (map snd)
