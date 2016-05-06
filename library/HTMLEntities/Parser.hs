-- |
-- Attoparsec parsers.
module HTMLEntities.Parser where

import HTMLEntities.Prelude
import Data.Attoparsec.Text
import qualified Data.Text as Text
import qualified HTMLEntities.NameTable as NameTable


-- |
-- A parser of a single entity.
-- 
-- Parses numeric encoding:
-- 
-- >>> mapM_ Data.Text.IO.putStrLn $ Data.Attoparsec.Text.parseOnly htmlEntity "&#169;"
-- ©
-- 
-- as well as named entities:
-- 
-- >>> mapM_ Data.Text.IO.putStrLn $ Data.Attoparsec.Text.parseOnly htmlEntity "&copy;"
-- ©
-- 
{-# INLINABLE htmlEntity #-}
htmlEntity :: Parser Text
htmlEntity =
  char '&' *> (numeric <|> named) <* char ';'
  where
    numeric =
      Text.singleton . chr <$> (char '#' *> (decimal <|> (char 'x' *> hexadecimal)))
    named =
      takeWhile1 isAlpha >>= maybe (fail "empty") string . NameTable.lookupTextByName
