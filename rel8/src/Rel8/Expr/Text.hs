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
import Prelude hiding (length, repeat, reverse)

-- rel8
import Rel8.Internal.Expr.Text 
