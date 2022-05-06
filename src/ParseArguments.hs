{-# LANGUAGE OverloadedStrings, TupleSections #-}
module ParseArguments
    ( parseColSpec
    , parseArguments
    , OutputColumns
    , Comparator
    ) where

import Control.Applicative ((<|>))
import Data.ByteString.Internal (c2w)
import Data.Word8 (isDigit, Word8(..))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

pcreSpecialCharacters = "\\|?*+.^${[()" :: BS.ByteString
import ColSpec


comma = c2w ','
equals = c2w '='
colon = c2w ':'
backslash = c2w '\\'
slash = c2w '/'

newtype UncompiledRegex = UncompiledRegex BS.ByteString
    deriving Show
type OutputColumns = [ColSpec]
type Comparator = ([ColSpec], UncompiledRegex)

regexEscape :: [Word8] -> [Word8]
regexEscape [] = []
regexEscape (c:cs)
  | c `elem` BS.unpack pcreSpecialCharacters = backslash:c:regexEscape cs
  | otherwise = c:regexEscape cs

parseLiteralMatch :: P.Parser UncompiledRegex
parseUncompiledRegex :: P.Parser UncompiledRegex
parseColRange :: P.Parser Range
parseColSpec :: P.Parser ColSpec
parseArgument :: P.Parser (Either OutputColumns Comparator)

parseUncompiledRegex = UncompiledRegex <$> regexString
  where regexString = BS.pack <$> (P.word8 slash >> P.manyTill regexChar (P.word8 slash))
        regexChar = P.word8 backslash *> P.word8 slash <|> P.anyWord8

parseLiteralMatch = do
  literal <- BS.pack <$> regexEscape <$> P.many1 escapedChar
  let regex = (c2w '^') `BS.cons` (literal `BS.snoc` (c2w '$'))
  return $ UncompiledRegex regex
  where escapedChar = P.word8 backslash *> P.satisfy special
                  <|> P.satisfy (not . endChar)
        special b = b == colon || b == backslash || endChar b
        endChar b = b == comma || b == equals

parseInteger :: P.Parser Int
parseInteger = do
  sign <- Just <$> P.word8 (c2w '-') <|> return Nothing
  digits <- P.takeWhile1 isDigit
  let string = C8.unpack $ maybe digits (`BS.cons` digits) sign
  return $ read string

parseRangeTo = P.word8 colon *> (RangeTo <$> parseInteger)
parseRangeFull = P.word8 colon *> return RangeFull
parseRange = do
  start <- parseInteger
  P.word8 colon
  end <- parseInteger
  return $ Range start end
parseRangeFrom = RangeFrom <$> parseInteger <* P.word8 colon

parseColRange = P.choice [parseRangeTo, parseRangeFull, parseRange, parseRangeFrom]

parseColSpec = ColRange <$> parseColRange
           <|> ColIndex <$> parseIndex
           <|> ColName <$> (parseUncompiledRegex <|> parseLiteralMatch)

parseArgument = do
  targetCols <- P.sepBy1 parseColSpec (P.word8 comma)
  finished <- P.atEnd
  if finished
    then return . Left $ TargetColumns targetCols
    else let comparison = (parseUncompiledRegex <|> parseLiteralMatch)
         in Right . Comparator . (TargetColumns targetCols,) <$> comparison
