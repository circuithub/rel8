module Rel8.Text where

import Data.ByteString
import Data.Int
import Data.Text (Text)
import Rel8.Expr (Expr, dbBinOp, dbFunction, nullaryFunction)

infixr 6 ++.
-- | The PostgreSQL string concatenation operator.
(++.) :: Expr Text -> Expr Text -> Expr Text
(++.) = dbBinOp "||"

-- * Regular expression operators

-- See https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP

infix 2 ~., ~*, !~, !~*

-- | Matches regular expression, case sensitive
(~.) :: Expr Text -> Expr Text -> Expr Bool
(~.) = dbBinOp "~."

-- | Matches regular expression, case insensitive
(~*) :: Expr Text -> Expr Text -> Expr Bool
(~*) = dbBinOp "~*"

-- | Does not match regular expression, case sensitive
(!~) :: Expr Text -> Expr Text -> Expr Bool
(!~) = dbBinOp "!~"

-- | Does not match regular expression, case insensitive
(!~*) :: Expr Text -> Expr Text -> Expr Bool
(!~*) = dbBinOp "!~*"

-- See https://www.postgresql.org/docs/9.5/static/functions-Expr.'PGHtml

-- * Standard SQL functions

bitLength :: Expr Text -> Expr Int32
bitLength = dbFunction "bit_length"

charLength :: Expr Text -> Expr Int32
charLength = dbFunction "char_length"

lower :: Expr Text -> Expr Text
lower = dbFunction "lower"

octetLength :: Expr Text -> Expr Int32
octetLength = dbFunction "octet_length"

upper :: Expr Text -> Expr Text
upper = dbFunction "upper"

-- * PostgreSQL functions

ascii :: Expr Text -> Expr Int32
ascii = dbFunction "ascii"

btrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
btrim a (Just b) = dbFunction "btrim" a b
btrim a Nothing = dbFunction "btrim" a

chr :: Expr Int32 -> Expr Text
chr = dbFunction "chr"

convert :: Expr ByteString -> Expr Text -> Expr Text -> Expr ByteString
convert = dbFunction "convert"

convertFrom :: Expr ByteString -> Expr Text -> Expr Text
convertFrom = dbFunction "convert_from"

convertTo :: Expr Text -> Expr Text -> Expr ByteString
convertTo = dbFunction "convert_to"

decode :: Expr Text -> Expr Text -> Expr ByteString
decode = dbFunction "decode"

encode :: Expr ByteString -> Expr Text -> Expr Text
encode = dbFunction "encode"

-- format :: Expr Text -> NonEmptyList (AnyExpr) -> Expr Text
-- format = _

initcap :: Expr Text -> Expr Text
initcap = dbFunction "initcap"

left :: Expr Text -> Expr Int32 -> Expr Text
left = dbFunction "left"

length :: Expr Text -> Expr Int32
length = dbFunction "length"

lengthEncoding :: Expr ByteString -> Expr Text -> Expr Int32
lengthEncoding = dbFunction "length"

lpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
lpad a b (Just c) = dbFunction "lpad" a b c
lpad a b Nothing = dbFunction "lpad" a b

ltrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
ltrim a (Just b) = dbFunction "ltrim" a b
ltrim a Nothing = dbFunction "ltrim" a

md5 :: Expr Text -> Expr Text
md5 = dbFunction "md5"

pgClientEncoding :: Expr Text
pgClientEncoding = nullaryFunction "pg_client_encoding"

quoteIdent :: Expr Text -> Expr Text
quoteIdent = dbFunction "quote_ident"

quoteLiteral :: Expr Text -> Expr Text
quoteLiteral = dbFunction "quote_literal"

quoteNullable :: Expr Text -> Expr Text
quoteNullable = dbFunction "quote_nullable"

regexpReplace :: Expr Text -> Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr Text
regexpReplace a b c (Just d) = dbFunction "regexp_replace" a b c d
regexpReplace a b c Nothing = dbFunction "regexp_replace" a b c

regexpSplitToArray :: Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr Text
regexpSplitToArray a b (Just c) = dbFunction "regexp_split_to_array" a b c
regexpSplitToArray a b Nothing = dbFunction "regexp_split_to_array" a b

repeat :: Expr Text -> Expr Int32 -> Expr Text
repeat = dbFunction "repeat"

replace :: Expr Text -> Expr Text -> Expr Text -> Expr Text
replace = dbFunction "replace"

reverse :: Expr Text -> Expr Text
reverse = dbFunction "reverse"

right :: Expr Text -> Expr Int32 -> Expr Text
right = dbFunction "right"

rpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
rpad a b (Just c) = dbFunction "rpad" a b c
rpad a b Nothing = dbFunction "rpad" a b

rtrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
rtrim a (Just b) = dbFunction "rtrim" a b
rtrim a Nothing = dbFunction "rtrim" a

splitPart :: Expr Text -> Expr Text -> Expr Int32 -> Expr Text
splitPart = dbFunction "split_part"

strpos :: Expr Text -> Expr Text -> Expr Int32
strpos = dbFunction "strpos"

substr :: Expr Text -> Expr Int32 -> Maybe (Expr Int32) -> Expr Text
substr a b (Just c) = dbFunction "substr" a b c
substr a b Nothing = dbFunction "substr" a b

toAscii :: Expr Text -> Expr Text -> Expr Text
toAscii = dbFunction "toAscii"

toHex :: Expr Int32 -> Expr Text
toHex = dbFunction "toHex"

translate :: Expr Text -> Expr Text -> Expr Text -> Expr Text
translate = dbFunction "translate"
