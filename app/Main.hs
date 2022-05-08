{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Monad(forM_, liftM2)
import Data.Bifunctor (second, bimap)
import Data.Bitraversable (bisequence)
import Data.List (foldl', partition)
import Data.Either
import Data.Maybe (fromMaybe)
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
type Comparators = M.Map FieldIndex [(Bool, RE.Regex)]

main :: IO ()
main = do
  args <- getArgs
  let parseResults = parseArguments args

  if any isLeft parseResults then
    forM_ (zip args parseResults) (\case
        (arg, Left err) -> putStrLn $ "Error parsing argument `" ++ arg ++ "': " ++ err
        _ -> return ())

  else let (outputColumns, comparators) = partitionEithers $ rights parseResults in
    process (concat outputColumns) comparators

process :: OutputColumns -> [Comparator] -> IO ()
process outputColumnSpecs  comparatorSpecs = do
    (firstLine, remainingLines) <- LC8.span (`notElem` recordSeparators) <$> LC8.getContents

    let columnNames = toFields firstLine
        numColumns = length columnNames

        -- thisisfine.jpg
        resolveColspec :: ColSpec.ColSpec -> IO (Result [FieldIndex])
        resolveColspecs :: [(Bool, ColSpec.ColSpec)]
                        -> IO (Result [(Bool, [FieldIndex])])
        resolveColspec = ColSpec.resolve columnNames numColumns
        resolveColspecs = seq2 <$> map (seq2 . second resolveColspec)


        resolveComparator :: ([(Bool, ColSpec.ColSpec)], (Bool, RE.UncompiledRegex))
                          -> IO (Result ([(Bool, [Int])], (Bool, RE.Regex)))
        resolveComparator =  biseq2 . bimap resolveColspecs (seq2 . second RE.compile)

        resolveComparators :: [Comparator] -> IO (Result [([(Bool, [Int])], (Bool, RE.Regex))])
        resolveComparators = seq2 <$> map resolveComparator

    resolvedOutputColumns <- resolveColspecs outputColumnSpecs
    resolvedComparators <- resolveComparators comparatorSpecs :: IO (Result [([(Bool, [FieldIndex])], (Bool, RE.Regex))])

    case (do
      outputColumns <- resolvedOutputColumns
      comparators <- resolvedComparators

      let outputColumns' = S.fromList $ resolveNegations numColumns outputColumns
          filterFields' = map (filterFields outputColumns')
          filterRecords' = filterRecords $ comparatorsByColumn numColumns comparators

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

resolveNegations :: FieldIndex -> [(Bool, [FieldIndex])] -> [FieldIndex]
resolveNegations _ [] = []
resolveNegations numFields specs = positive' `subtract` negative

  where (negative, positive) = bimapBoth (>>= snd) $ partition fst specs
        positive' = if null positive then [1..numFields] else positive
        subtract = foldl' removeFrom
        removeFrom l x = filter (/=x) l


comparatorsByColumn :: FieldIndex -> [([(Bool, [FieldIndex])], (Bool, RE.Regex))] -> Comparators
comparatorsByColumn numFields = foldl' addToMap M.empty
  where addToMap m (ix, re) = foldl' (\m i -> M.insertWith (++) i [re] m) m (resolveNegations numFields ix)


type MatchResult = Maybe Bool
filterRecords :: Comparators  -> [Record] -> [Record]
filterRecords comparators = filter recordMatches

  where recordMatches :: Record -> Bool
        recordMatches record =
          let matchResults = map fieldMatches $ zip [1..] record
              anyFieldMatches = or <$> sequence matchResults in
            fromMaybe False anyFieldMatches

        fieldMatches :: (FieldIndex, Field) -> MatchResult
        fieldMatches (fieldIndex, field) =
          let allRegexes = M.findWithDefault [] fieldIndex comparators
              (negative, positive) = bimapBoth (map snd) $ partition fst allRegexes
              matchAny' rs = matchAny rs [field]
            in if matchAny' negative then Nothing else Just $ matchAny' positive

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

bimapBoth f = bimap f f

seq2 :: (Monad m, Monad n, Traversable t) => t (m (n  a)) -> m (n (t a))
seq2 x = sequence <$> sequence x

biseq2 x = bisequence <$> bisequence x

xor :: Bool -> Bool -> Bool
xor a b = a /= b

matchMany :: [RE.Regex] -> [C8.ByteString] -> [Bool]
matchMany = liftM2 matchTest

matchAny rs ss = or $ matchMany rs ss
