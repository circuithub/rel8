{-# language DataKinds #-}

module Rel8.Expr.Text
  (
    -- * String concatenation
    (++.)

    -- * Regular expression operators
  , (~.), (~*), (!~), (!~*)

    -- * Standard SQL functions
  , bitLength, charLength, lower, octetLength, upper

    -- * PostgreSQL functions
  , ascii, btrim, chr, convert, convertFrom, convertTo, decode, encode
  , initcap, left, length, lengthEncoding, lpad, ltrim, md5
  , pgClientEncoding, quoteIdent, quoteLiteral, quoteNullable, regexpReplace
  , regexpSplitToArray, repeat, replace, reverse, right, rpad, rtrim
  , splitPart, strpos, substr, toAscii, toHex, translate
  )
where

-- base
import Data.Bool ( Bool )
import Data.Int ( Int32 )
import Data.Maybe ( Maybe( Nothing, Just ) )
import Prelude ()

-- bytestring
import Data.ByteString ( ByteString )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Function ( binaryOperator, function, nullaryFunction )
import Rel8.Kind.Emptiability ( Emptiability( NonEmptiable ) )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type.Array ( Array )

-- text
import Data.Text (Text)


-- | The PostgreSQL string concatenation operator.
(++.) :: Expr nullability Text -> Expr nullability Text -> Expr nullability Text
(++.) = binaryOperator "||"
infixr 6 ++.


-- * Regular expression operators

-- See https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP


-- | Matches regular expression, case sensitive
(~.) :: Expr nullability Text -> Expr nullability Text -> Expr nullability Bool
(~.) = binaryOperator "~."
infix 2 ~.


-- | Matches regular expression, case insensitive
(~*) :: Expr nullability Text -> Expr nullability Text -> Expr nullability Bool
(~*) = binaryOperator "~*"
infix 2 ~*


-- | Does not match regular expression, case sensitive
(!~) :: Expr nullability Text -> Expr nullability Text -> Expr nullability Bool
(!~) = binaryOperator "!~"
infix 2 !~


-- | Does not match regular expression, case insensitive
(!~*) :: Expr nullability Text -> Expr nullability Text -> Expr nullability Bool
(!~*) = binaryOperator "!~*"
infix 2 !~*


-- See https://www.postgresql.org/docs/9.5/static/functions-Expr nullability.'PGHtml

-- * Standard SQL functions


bitLength :: Expr nullability Text -> Expr nullability Int32
bitLength = function "bit_length"


charLength :: Expr nullability Text -> Expr nullability Int32
charLength = function "char_length"


lower :: Expr nullability Text -> Expr nullability Text
lower = function "lower"


octetLength :: Expr nullability Text -> Expr nullability Int32
octetLength = function "octet_length"


upper :: Expr nullability Text -> Expr nullability Text
upper = function "upper"


-- * PostgreSQL functions
ascii :: Expr nullability Text -> Expr nullability Int32
ascii = function "ascii"


btrim :: ()
  => Expr nullability Text
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
btrim a (Just b) = function "btrim" a b
btrim a Nothing = function "btrim" a


chr :: Expr nullability Int32 -> Expr nullability Text
chr = function "chr"


convert :: ()
  => Expr nullability ByteString
  -> Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability ByteString
convert = function "convert"


convertFrom :: ()
  => Expr nullability ByteString
  -> Expr nullability Text
  -> Expr nullability Text
convertFrom = function "convert_from"


convertTo :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability ByteString
convertTo = function "convert_to"


decode :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability ByteString
decode = function "decode"


encode :: ()
  => Expr nullability ByteString
  -> Expr nullability Text
  -> Expr nullability Text
encode = function "encode"


initcap :: Expr nullability Text -> Expr nullability Text
initcap = function "initcap"


left :: ()
  => Expr nullability Text -> Expr nullability Int32 -> Expr nullability Text
left = function "left"


length :: Expr nullability Text -> Expr nullability Int32
length = function "length"


lengthEncoding :: ()
  => Expr nullability ByteString -> Expr nullability Text
  -> Expr nullability Int32
lengthEncoding = function "length"


lpad :: ()
  => Expr nullability Text
  -> Expr nullability Int32
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
lpad a b (Just c) = function "lpad" a b c
lpad a b Nothing = function "lpad" a b


ltrim :: ()
  => Expr nullability Text
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
ltrim a (Just b) = function "ltrim" a b
ltrim a Nothing = function "ltrim" a


md5 :: Expr nullability Text -> Expr nullability Text
md5 = function "md5"


pgClientEncoding :: Expr nullability Text
pgClientEncoding = nullaryFunction "pg_client_encoding"


quoteIdent :: Expr nullability Text -> Expr nullability Text
quoteIdent = function "quote_ident"


quoteLiteral :: Expr nullability Text -> Expr nullability Text
quoteLiteral = function "quote_literal"


quoteNullable :: Expr nullability Text -> Expr nullability Text
quoteNullable = function "quote_nullable"


regexpReplace :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
regexpReplace a b c (Just d) = function "regexp_replace" a b c d
regexpReplace a b c Nothing = function "regexp_replace" a b c


regexpSplitToArray :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Maybe (Expr nullability Text)
  -> Expr nullability (Array 'NonEmptiable 'NonNullable Text)
regexpSplitToArray a b (Just c) = function "regexp_split_to_array" a b c
regexpSplitToArray a b Nothing = function "regexp_split_to_array" a b


repeat :: ()
  => Expr nullability Text
  -> Expr nullability Int32
  -> Expr nullability Text
repeat = function "repeat"


replace :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
replace = function "replace"


reverse :: Expr nullability Text -> Expr nullability Text
reverse = function "reverse"


right :: ()
  => Expr nullability Text
  -> Expr nullability Int32
  -> Expr nullability Text
right = function "right"


rpad :: ()
  => Expr nullability Text
  -> Expr nullability Int32
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
rpad a b (Just c) = function "rpad" a b c
rpad a b Nothing = function "rpad" a b


rtrim :: ()
  => Expr nullability Text
  -> Maybe (Expr nullability Text)
  -> Expr nullability Text
rtrim a (Just b) = function "rtrim" a b
rtrim a Nothing = function "rtrim" a


splitPart :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Int32
  -> Expr nullability Text
splitPart = function "split_part"


strpos :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Int32
strpos = function "strpos"


substr :: ()
  => Expr nullability Text
  -> Expr nullability Int32
  -> Maybe (Expr nullability Int32)
  -> Expr nullability Text
substr a b (Just c) = function "substr" a b c
substr a b Nothing = function "substr" a b


toAscii :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
toAscii = function "toAscii"


toHex :: Expr nullability Int32 -> Expr nullability Text
toHex = function "toHex"


translate :: ()
  => Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
  -> Expr nullability Text
translate = function "translate"
