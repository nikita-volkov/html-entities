-- |
-- Attoparsec parsers.
module HTMLEntities.Parser where

import HTMLEntities.Prelude
import Data.Attoparsec.Text
import qualified Data.Text as Text
import qualified HTMLEntities.NamedTable as NamedTable


-- |
-- A parser of a single entity.
-- 
-- >>> parseOnly htmlEntity "&copy;"
-- Right "\169"
-- 
-- >>> parseOnly htmlEntity "&#169;"
-- Right "\169"
{-# INLINABLE htmlEntity #-}
htmlEntity :: Parser Text
htmlEntity =
  char '&' *> (numeric <|> named) <* char ';'
  where
    numeric =
      Text.singleton . chr <$> (char '#' *> (decimal <|> (char 'x' *> hexadecimal)))
    named =
      takeWhile1 isAlpha >>= liftMaybe . NamedTable.lookup
    liftMaybe =
      maybe empty pure
