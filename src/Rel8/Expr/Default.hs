module Rel8.Expr.Default
  ( unsafeDefault
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr )


-- | Corresponds to the SQL @DEFAULT@ expression.
--
-- This 'Expr' is unsafe for numerous reasons, and should be used with care:
--
-- 1. This 'Expr' only makes sense in an @INSERT@ or @UPDATE@ statement.
--
-- 2. Rel8 is not able to verify that a particular column actually has a
-- @DEFAULT@ value. Trying to use @unsafeDefault@ where there is no default
-- will cause a runtime crash
--
-- 3. @DEFAULT@ values can not be transformed. For example, the innocuous Rel8
-- code @unsafeDefault + 1@ will crash, despite type checking.
--
-- Also note, PostgreSQL's syntax rules mean that @DEFAULT@ can only appear in
-- @INSERT@ expressions whose rows are specified using @VALUES@. This means
-- that if the @rows@ field of your 'Rel8.Insert' record doesn\'t look like
-- @values [..]@, then @unsafeDefault@ won't work.
--
-- Given all these caveats, we suggest avoiding the use of default values where
-- possible, instead being explicit. A common scenario where default values are
-- used is with auto-incrementing identifier columns. In this case, we suggest
-- using 'Rel8.nextval' instead.
unsafeDefault :: Expr a
unsafeDefault = fromPrimExpr Opaleye.DefaultInsertExpr
