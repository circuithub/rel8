{-# language DataKinds #-}
{-# language OverloadedStrings #-}

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
  , splitPart, strpos, substr, translate

    -- * @LIKE@ and @ILIKE@
  , like, ilike
  )
where

-- base
import Data.Bool ( Bool )
import Data.Int ( Int32 )
import Data.Maybe ( Maybe( Nothing, Just ) )
import Prelude ( flip )

-- bytestring
import Data.ByteString ( ByteString )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Function (binaryOperator, function)

-- text
import Data.Text (Text)


-- | The PostgreSQL string concatenation operator.
(++.) :: Expr Text -> Expr Text -> Expr Text
(++.) = binaryOperator "||"
infixr 6 ++.


-- * Regular expression operators

-- See https://www.postgresql.org/docs/9.5/static/functions-matching.html#FUNCTIONS-POSIX-REGEXP


-- | Matches regular expression, case sensitive
--
-- Corresponds to the @~.@ operator.
(~.) :: Expr Text -> Expr Text -> Expr Bool
(~.) = binaryOperator "~."
infix 2 ~.


-- | Matches regular expression, case insensitive
--
-- Corresponds to the @~*@ operator.
(~*) :: Expr Text -> Expr Text -> Expr Bool
(~*) = binaryOperator "~*"
infix 2 ~*


-- | Does not match regular expression, case sensitive
--
-- Corresponds to the @!~@ operator.
(!~) :: Expr Text -> Expr Text -> Expr Bool
(!~) = binaryOperator "!~"
infix 2 !~


-- | Does not match regular expression, case insensitive
--
-- Corresponds to the @!~*@ operator.
(!~*) :: Expr Text -> Expr Text -> Expr Bool
(!~*) = binaryOperator "!~*"
infix 2 !~*


-- See https://www.postgresql.org/docs/9.5/static/functions-Expr.'PGHtml

-- * Standard SQL functions


-- | Corresponds to the @bit_length@ function.
bitLength :: Expr Text -> Expr Int32
bitLength = function "bit_length"


-- | Corresponds to the @char_length@ function.
charLength :: Expr Text -> Expr Int32
charLength = function "char_length"


-- | Corresponds to the @lower@ function.
lower :: Expr Text -> Expr Text
lower = function "lower"


-- | Corresponds to the @octet_length@ function.
octetLength :: Expr Text -> Expr Int32
octetLength = function "octet_length"


-- | Corresponds to the @upper@ function.
upper :: Expr Text -> Expr Text
upper = function "upper"


-- | Corresponds to the @ascii@ function.
ascii :: Expr Text -> Expr Int32
ascii = function "ascii"


-- | Corresponds to the @btrim@ function.
btrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
btrim a (Just b) = function "btrim" (a, b)
btrim a Nothing = function "btrim" a


-- | Corresponds to the @chr@ function.
chr :: Expr Int32 -> Expr Text
chr = function "chr"


-- | Corresponds to the @convert@ function.
convert :: Expr ByteString -> Expr Text -> Expr Text -> Expr ByteString
convert a b c = function "convert" (a, b, c)


-- | Corresponds to the @convert_from@ function.
convertFrom :: Expr ByteString -> Expr Text -> Expr Text
convertFrom a b = function "convert_from" (a, b)


-- | Corresponds to the @convert_to@ function.
convertTo :: Expr Text -> Expr Text -> Expr ByteString
convertTo a b = function "convert_to" (a, b)


-- | Corresponds to the @decode@ function.
decode :: Expr Text -> Expr Text -> Expr ByteString
decode a b = function "decode" (a, b)


-- | Corresponds to the @encode@ function.
encode :: Expr ByteString -> Expr Text -> Expr Text
encode a b = function "encode" (a, b)


-- | Corresponds to the @initcap@ function.
initcap :: Expr Text -> Expr Text
initcap = function "initcap"


-- | Corresponds to the @left@ function.
left :: Expr Text -> Expr Int32 -> Expr Text
left a b = function "left" (a, b)


-- | Corresponds to the @length@ function.
length :: Expr Text -> Expr Int32
length = function "length"


-- | Corresponds to the @length@ function.
lengthEncoding :: Expr ByteString -> Expr Text -> Expr Int32
lengthEncoding a b = function "length" (a, b)


-- | Corresponds to the @lpad@ function.
lpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
lpad a b (Just c) = function "lpad" (a, b, c)
lpad a b Nothing = function "lpad" (a, b)


-- | Corresponds to the @ltrim@ function.
ltrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
ltrim a (Just b) = function "ltrim" (a, b)
ltrim a Nothing = function "ltrim" a


-- | Corresponds to the @md5@ function.
md5 :: Expr Text -> Expr Text
md5 = function "md5"


-- | Corresponds to the @pg_client_encoding()@ expression.
pgClientEncoding :: Expr Text
pgClientEncoding = function "pg_client_encoding" ()


-- | Corresponds to the @quote_ident@ function.
quoteIdent :: Expr Text -> Expr Text
quoteIdent = function "quote_ident"


-- | Corresponds to the @quote_literal@ function.
quoteLiteral :: Expr Text -> Expr Text
quoteLiteral = function "quote_literal"


-- | Corresponds to the @quote_nullable@ function.
quoteNullable :: Expr Text -> Expr Text
quoteNullable = function "quote_nullable"


-- | Corresponds to the @regexp_replace@ function.
regexpReplace :: ()
  => Expr Text -> Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr Text
regexpReplace a b c (Just d) = function "regexp_replace" (a, b, c, d)
regexpReplace a b c Nothing = function "regexp_replace" (a, b, c)


-- | Corresponds to the @regexp_split_to_array@ function.
regexpSplitToArray :: ()
  => Expr Text -> Expr Text -> Maybe (Expr Text) -> Expr [Text]
regexpSplitToArray a b (Just c) = function "regexp_split_to_array" (a, b, c)
regexpSplitToArray a b Nothing = function "regexp_split_to_array" (a, b)


-- | Corresponds to the @repeat@ function.
repeat :: Expr Text -> Expr Int32 -> Expr Text
repeat a b = function "repeat" (a, b)


-- | Corresponds to the @replace@ function.
replace :: Expr Text -> Expr Text -> Expr Text -> Expr Text
replace a b c = function "replace" (a, b, c)


-- | Corresponds to the @reverse@ function.
reverse :: Expr Text -> Expr Text
reverse = function "reverse"


-- | Corresponds to the @right@ function.
right :: Expr Text -> Expr Int32 -> Expr Text
right a b = function "right" (a, b)


-- | Corresponds to the @rpad@ function.
rpad :: Expr Text -> Expr Int32 -> Maybe (Expr Text) -> Expr Text
rpad a b (Just c) = function "rpad" (a, b, c)
rpad a b Nothing = function "rpad" (a, b)


-- | Corresponds to the @rtrim@ function.
rtrim :: Expr Text -> Maybe (Expr Text) -> Expr Text
rtrim a (Just b) = function "rtrim" (a, b)
rtrim a Nothing = function "rtrim" a


-- | Corresponds to the @split_part@ function.
splitPart :: Expr Text -> Expr Text -> Expr Int32 -> Expr Text
splitPart a b c = function "split_part" (a, b, c)


-- | Corresponds to the @strpos@ function.
strpos :: Expr Text -> Expr Text -> Expr Int32
strpos a b = function "strpos" (a, b)


-- | Corresponds to the @substr@ function.
substr :: Expr Text -> Expr Int32 -> Maybe (Expr Int32) -> Expr Text
substr a b (Just c) = function "substr" (a, b, c)
substr a b Nothing = function "substr" (a, b)


-- | Corresponds to the @translate@ function.
translate :: Expr Text -> Expr Text -> Expr Text -> Expr Text
translate a b c = function "translate" (a, b, c)


-- | @like x y@ corresponds to the expression @y LIKE x@.
--
-- Note that the arguments to @like@ are swapped. This is to aid currying, so
-- you can write expressions like
-- @filter (like "Rel%" . packageName) =<< each haskellPackages@
like :: Expr Text -> Expr Text -> Expr Bool
like = flip (binaryOperator "LIKE")


-- | @ilike x y@ corresponds to the expression @y ILIKE x@.
--
-- Note that the arguments to @ilike@ are swapped. This is to aid currying, so
-- you can write expressions like
-- @filter (ilike "Rel%" . packageName) =<< each haskellPackages@
ilike :: Expr Text -> Expr Text -> Expr Bool
ilike = flip (binaryOperator "ILIKE")
