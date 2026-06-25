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

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Function (binaryOperator, function)
import Rel8.Internal.Expr.Opaleye (zipPrimExprsWith)
import Rel8.Internal.Expr.Text 
