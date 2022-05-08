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

import ColSpec
import Result

pcreSpecialCharacters = BS.unpack "\\|?*+.^${[()"

comma = c2w ','
equals = c2w '='
colon = c2w ':'
backslash = c2w '\\'
slash = c2w '/'
exclamationMark = c2w '!'

type OutputColumns = [(Bool, ColSpec)]
type Comparator = ([(Bool, ColSpec)], (Bool, UncompiledRegex))

regexEscape :: [Word8] -> [Word8]
regexEscape [] = []
regexEscape (c:cs)
  | c `elem` pcreSpecialCharacters = backslash:c:regexEscape cs
  | otherwise = c:regexEscape cs

parseLiteralMatch :: P.Parser UncompiledRegex
parseUncompiledRegex :: P.Parser UncompiledRegex
parseColRange :: P.Parser Range
parseColSpec :: P.Parser (Bool, ColSpec)
parseArgument :: P.Parser (Either OutputColumns Comparator)
parseArguments :: [String] -> [Result (Either OutputColumns Comparator)]

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
parseIndex = parseInteger

parseRangeTo = P.word8 colon *> (RangeTo <$> parseIndex)
parseRangeFull = P.word8 colon *> return RangeFull
parseRange = do
  start <- parseIndex
  P.word8 colon
  end <- parseIndex
  return $ Range start end
parseRangeFrom = RangeFrom <$> parseIndex <* P.word8 colon

parseColRange = P.choice [parseRangeTo, parseRangeFull, parseRange, parseRangeFrom]

parseColSpec = withNegation matchRule
  where matchRule = ColRange <$> parseColRange
                <|> ColIndex <$> parseIndex
                <|> ColName <$> (parseUncompiledRegex <|> parseLiteralMatch)

withNegation :: P.Parser p -> P.Parser (Bool, p)
withNegation p = (,) <$> negation <*> p
  where negation = (P.word8 exclamationMark >> return True) <|> return False

parseArgument = do
  targetCols <- P.sepBy1 parseColSpec (P.word8 comma)
  finished <- P.atEnd
  if finished
    then return $ Left targetCols
    else let comparison = (P.word8 equals P.<?> "Expected '=' (ambiguous parse?)")
                          *> withNegation (parseUncompiledRegex <|> parseLiteralMatch)
         in Right . (targetCols,) <$> comparison

parseArguments = map (P.parseOnly parseArgument . C8.pack)
