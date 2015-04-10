-- |
-- Utilities, which execute the parser.
module HTMLEntities.Decoder where

import HTMLEntities.Prelude
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Attoparsec.Text as P
import qualified HTMLEntities.Parser as P


type Decoder a =
  Text -> Either String a

-- |
-- A decoder of a single entity.
-- 
-- >>> htmlEntity "&#169;"
-- Right "\169"
htmlEntity :: Decoder Text
htmlEntity =
  P.parseOnly $
  P.htmlEntity <* P.endOfInput

-- |
-- A decoder of a text with entities.
-- 
-- Produces a text builder, 
-- which you can then convert into a text or a lazy text,
-- using the \"text\" library.
-- 
-- >>> fmap TLB.toLazyText $ htmlEncodedText "&euro;5 &cent;2"
-- Right "\8364\&5 \162\&2"
htmlEncodedText :: Decoder TLB.Builder
htmlEncodedText =
  P.parseOnly $
  fmap fold $
  many $
  fmap TLB.fromText P.htmlEntity <|> fmap TLB.singleton P.anyChar

