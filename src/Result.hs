module Result
  ( Result(..)
  , allSucceed
  , bothSucceed
  ) where

import Data.Bitraversable
type Result = Either String

allSucceed :: [Result [a]] -> Result [a]
allSucceed [] = Right []
allSucceed results = foldr1 concatResult results
  where concatResult err@(Left _) _ = err
        concatResult x a = (++) <$> x <*> a

bothSucceed :: (Bitraversable t) => t (Result a) (Result b) -> Result (t a b)
bothSucceed = bisequence
