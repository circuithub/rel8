module Rel8.Text
  ( (++.)
  , (~.)
  , (~*)
  , (!~)
  , (!~*)
  , bitLength
  , charLength
  , lower
  , octetLength
  , upper
  , ascii
  , btrim
  , chr
  , convert
  , convertFrom
  , convertTo
  , decode
  , encode
  , initcap
  , left
  , Rel8.Text.length
  , lengthEncoding
  , lpad
  , ltrim
  , md5
  , pgClientEncoding
  , quoteIdent
  , quoteLiteral
  , quoteNullable
  , regexpReplace
  , regexpSplitToArray
  , Rel8.Text.repeat
  , replace
  , Rel8.Text.reverse
  , right
  , rpad
  , rtrim
  , splitPart
  , strpos
  , substr
  , toAscii
  , toHex
  , translate
  ) where

-- base
import Data.Int ( Int32 )

-- bytestring
import Data.ByteString ( ByteString )

-- rel8
import Rel8 ( Expr, binaryOperator, function, nullaryFunction )

-- text
import Data.Text ( Text )


infixr 6 ++.


-- | The PostgreSQL string concatenation operator.
(++.) :: Expr Text -> Expr Text -> Expr Text
(++.) = binaryOperator "||"


-- * Regular expression operators

-- See https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP
infix 2 ~., ~*, !~, !~*


-- | Matches regular expression, case sensitive
(~.) :: Expr Text -> Expr Text -> Expr Bool
(~.) = binaryOperator "~."


-- | Matches regular expression, case insensitive
(~*) :: Expr Text -> Expr Text -> Expr Bool
(~*) = binaryOperator "~*"


-- | Does not match regular expression, case sensitive
(!~) :: Expr Text -> Expr Text -> Expr Bool
(!~) = binaryOperator "!~"


-- | Does not match regular expression, case insensitive
(!~*) :: Expr Text -> Expr Text -> Expr Bool
(!~*) = binaryOperator "!~*"


-- See https://www.postgresql.org/docs/9.5/static/functions-Expr.'PGHtml

-- * Standard SQL functions
bitLength :: Expr Text -> Expr Int32
bitLength = function "bit_length"


charLength :: Expr Text -> Expr Int32
charLength = function "char_length"


lower :: Expr Text -> Expr Text
lower = function "lower"


octetLength :: Expr Text -> Expr Int32
octetLength = function "octet_length"


upper :: Expr Text -> Expr Text
upper = function "upper"


-- * PostgreSQL functions
ascii :: Expr Text -> Expr Int32
ascii = function "ascii"


btrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
btrim a (Just b) = function "btrim" a b
btrim a Nothing = function "btrim" a


chr :: Expr Int32 -> Expr Text
chr = function "chr"


convert :: Expr ByteString -> Expr Text -> Expr Text -> Expr ByteString
convert = function "convert"


convertFrom :: Expr ByteString -> Expr Text -> Expr Text
convertFrom = function "convert_from"


convertTo :: Expr Text -> Expr Text -> Expr ByteString
convertTo = function "convert_to"


decode :: Expr Text -> Expr Text -> Expr ByteString
decode = function "decode"


encode :: Expr ByteString -> Expr Text -> Expr Text
encode = function "encode"


-- format :: Expr Text -> NonEmptyList (AnyExpr) -> Expr Text
-- format = _
initcap :: Expr Text -> Expr Text
initcap = function "initcap"


left :: Expr Text -> Expr Int32 -> Expr Text
left = function "left"


length :: Expr Text -> Expr Int32
length = function "length"


lengthEncoding :: Expr ByteString -> Expr Text -> Expr Int32
lengthEncoding = function "length"


lpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
lpad a b (Just c) = function "lpad" a b c
lpad a b Nothing = function "lpad" a b


ltrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
ltrim a (Just b) = function "ltrim" a b
ltrim a Nothing = function "ltrim" a


md5 :: Expr Text -> Expr Text
md5 = function "md5"


pgClientEncoding :: Expr Text
pgClientEncoding = nullaryFunction "pg_client_encoding"


quoteIdent :: Expr Text -> Expr Text
quoteIdent = function "quote_ident"


quoteLiteral :: Expr Text -> Expr Text
quoteLiteral = function "quote_literal"


quoteNullable :: Expr Text -> Expr Text
quoteNullable = function "quote_nullable"


regexpReplace :: Expr Text -> Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr Text
regexpReplace a b c (Just d) = function "regexp_replace" a b c d
regexpReplace a b c Nothing = function "regexp_replace" a b c


regexpSplitToArray :: Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr Text
regexpSplitToArray a b (Just c) = function "regexp_split_to_array" a b c
regexpSplitToArray a b Nothing = function "regexp_split_to_array" a b


repeat :: Expr Text -> Expr Int32 -> Expr Text
repeat = function "repeat"


replace :: Expr Text -> Expr Text -> Expr Text -> Expr Text
replace = function "replace"


reverse :: Expr Text -> Expr Text
reverse = function "reverse"


right :: Expr Text -> Expr Int32 -> Expr Text
right = function "right"


rpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
rpad a b (Just c) = function "rpad" a b c
rpad a b Nothing = function "rpad" a b


rtrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
rtrim a (Just b) = function "rtrim" a b
rtrim a Nothing = function "rtrim" a


splitPart :: Expr Text -> Expr Text -> Expr Int32 -> Expr Text
splitPart = function "split_part"


strpos :: Expr Text -> Expr Text -> Expr Int32
strpos = function "strpos"


substr :: Expr Text -> Expr Int32 -> Maybe (Expr Int32) -> Expr Text
substr a b (Just c) = function "substr" a b c
substr a b Nothing = function "substr" a b


toAscii :: Expr Text -> Expr Text -> Expr Text
toAscii = function "toAscii"


toHex :: Expr Int32 -> Expr Text
toHex = function "toHex"


translate :: Expr Text -> Expr Text -> Expr Text -> Expr Text
translate = function "translate"
