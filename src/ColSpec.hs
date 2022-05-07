{-# LANGUAGE ApplicativeDo #-}
module ColSpec
  ( ColSpec(..)
  , Range(..)
  , UncompiledRegex(..)
  , resolve
  ) where

import Data.Function((&))
import Data.List (findIndices)
import Data.Ix (inRange)
import qualified Data.ByteString as BS
import qualified Text.Regex.Base as RE

import Regex

data ColSpec = ColRange Range | ColIndex Int | ColName UncompiledRegex
    deriving Show

data Range = RangeFull | RangeTo Int | RangeFrom Int | Range Int Int
    deriving Show

type Result = Either String

indices :: Int -> Range -> [Int]
indices maxIndex range = case range of
  RangeFull -> [0 .. maxIndex]
  RangeTo i -> [0 .. to i]
  RangeFrom i -> [from i .. maxIndex]
  Range i0 i1 -> [from i0 .. to i1]
  where from = max 0 . wrap
        to = min maxIndex . wrap
        wrap i = if i >= 0 then i else maxIndex + 1 + i


resolve :: [BS.ByteString] -> Int -> ColSpec -> IO (Result [Int])
resolve _ maxIndex (ColRange range) = return . Right $ indices maxIndex range

resolve _ maxIndex (ColIndex i) = return $
  if i & inRange (1, maxIndex)
  then Right [i]
  else Left $ concat ["Invalid index: ", show i,
                       " not within [1, ", show maxIndex, "]"]

resolve colNames _ (ColName regex) = do
  -- TODO: Error if no matches
  regex <- compile regex
  return $ findMatchIndices colNames <$> regex

findMatchIndices :: [BS.ByteString] -> Regex -> [Int]
findMatchIndices strings regex = findIndices' (RE.matchTest regex) strings
    where findIndices' p xs = map succ $ findIndices p xs
