module Regex
  ( UncompiledRegex(..)
  , PCRE.Regex(..)
  , compile
  ) where

import Data.Bifunctor
import qualified Text.Regex.PCRE.ByteString as PCRE
import qualified Data.ByteString as BS
import Result

newtype UncompiledRegex = UncompiledRegex BS.ByteString
    deriving Show

-- TODO: flags to enable caseless, nl_type, UCP, multiline, UTF check
compOptions = PCRE.compUTF8 + PCRE.compNoUTF8Check + PCRE.compMultiline
execOptions = PCRE.execNoUTF8Check

compile :: UncompiledRegex -> IO (Result PCRE.Regex)
compile (UncompiledRegex r) = (first snd) <$> PCRE.compile compOptions execOptions r
