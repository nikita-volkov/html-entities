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
-- as well as the named entities:
-- 
-- >>> mapM_ Data.Text.IO.putStrLn $ Data.Attoparsec.Text.parseOnly htmlEntity "&copy;"
-- ©
-- 
{-# INLINE htmlEntity #-}
htmlEntity :: Parser Text
htmlEntity =
  char '&' *> htmlEntityBody <* char ';'

-- |
-- A parser of the body of a single entity.
-- 
-- Parses numeric encoding:
-- 
-- >>> mapM_ Data.Text.IO.putStrLn $ Data.Attoparsec.Text.parseOnly htmlEntityBody "#169"
-- ©
-- 
-- as well as the named entities:
-- 
-- >>> mapM_ Data.Text.IO.putStrLn $ Data.Attoparsec.Text.parseOnly htmlEntityBody "copy"
-- ©
-- 
{-# INLINABLE htmlEntityBody #-}
htmlEntityBody :: Parser Text
htmlEntityBody =
  numeric <|> named
  where
    numeric =
      Text.singleton . chr <$> (char '#' *> (decimal <|> (char 'x' *> hexadecimal)))
    named =
      takeWhile1 isAlpha >>= liftMaybe . NameTable.lookupTextByName
    liftMaybe =
      maybe empty pure
