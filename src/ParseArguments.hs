{-# LANGUAGE OverloadedStrings, TupleSections #-}
module ParseArguments
    ( parseColSpec
    , parseArguments
    , OutputColumns
    , Comparator
    ) where

import Control.Applicative ((<|>))
import Data.ByteString.Internal (c2w)
import Data.Word8
  ( isDigit
  , Word8(..)
  , _backslash, _circum, _colon, _comma, _dollar, _equal, _exclam, _hyphen, _slash, _tilde
  )
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import ColSpec
import Result

pcreSpecialCharacters = BS.unpack "\\|?*+.^${[()"

type OutputColumns = [(Bool, ColSpec)]
type Comparator = ([(Bool, ColSpec)], (Bool, UncompiledRegex))

regexEscape :: [Word8] -> [Word8]
regexEscape [] = []
regexEscape (c:cs)
  | c `elem` pcreSpecialCharacters = _backslash:c:regexEscape cs
  | otherwise = c:regexEscape cs

parseLiteralMatch :: P.Parser UncompiledRegex
parseUncompiledRegex :: P.Parser UncompiledRegex
parseColRange :: P.Parser Range
parseColSpec :: P.Parser (Bool, ColSpec)
parseArgument :: P.Parser (Either OutputColumns Comparator)
parseArguments :: [String] -> [Result (Either OutputColumns Comparator)]

parseUncompiledRegex = UncompiledRegex <$> regexString
  where regexString = BS.pack <$> (P.word8 _slash >> P.manyTill regexChar (P.word8 _slash))
        regexChar = P.word8 _backslash *> P.word8 _slash <|> P.anyWord8

parseLiteralMatch = do
  literal <- BS.pack <$> regexEscape <$> P.many1 escapedChar
  let regex = _circum `BS.cons` (literal `BS.snoc` _dollar)
  return $ UncompiledRegex regex
  where escapedChar = P.word8 _backslash *> P.satisfy special
                  <|> P.satisfy (not . endChar)
        special b = b == _colon || b == _backslash || endChar b
        endChar b = b == _comma || b == _equal

parseInteger :: P.Parser Int
parseInteger = do
  sign <- Just <$> P.word8 (c2w '-') <|> return Nothing
  digits <- P.takeWhile1 isDigit
  let string = C8.unpack $ maybe digits (`BS.cons` digits) sign
  return $ read string
parseIndex = parseInteger

parseRangeTo = P.word8 _colon *> (RangeTo <$> parseIndex)
parseRangeFull = P.word8 _colon *> return RangeFull
parseRange = do
  start <- parseIndex
  P.word8 _colon
  end <- parseIndex
  return $ Range start end
parseRangeFrom = RangeFrom <$> parseIndex <* P.word8 _colon

parseColRange = P.choice [parseRangeTo, parseRangeFull, parseRange, parseRangeFrom]

parseColSpec = withNegation matchRule
  where matchRule = ColRange <$> parseColRange
                <|> ColIndex <$> parseIndex
                <|> ColName <$> (parseUncompiledRegex <|> parseLiteralMatch)

withNegation :: P.Parser p -> P.Parser (Bool, p)
withNegation p = (,) <$> negation <*> p
  where negation = (P.word8 _exclam >> return True) <|> return False

parseArgument = do
  targetCols <- P.sepBy1 parseColSpec (P.word8 _comma)
  finished <- P.atEnd
  if finished
    then return $ Left targetCols
    else let comparison = (P.word8 _equal P.<?> "Expected '=' (ambiguous parse?)")
                          *> withNegation (parseUncompiledRegex <|> parseLiteralMatch)
         in Right . (targetCols,) <$> comparison

parseArguments = map (P.parseOnly parseArgument . C8.pack)
