{-# language FlexibleInstances #-}
{-# language KindSignatures #-}

module Rel8.DBType.DBEq ( DBEq(..) ) where

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Type )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( binExpr )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy

-- time
import Data.Time ( Day, UTCTime )


-- | Database column types that can be compared for equality in queries.
-- 
-- Usually, this means producing an expression using the (overloaded) @=@
-- operator, but types can provide a more elaborate expression if necessary.
-- 
-- [ @DBEq@ with @newtype@s ]
-- 
-- Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype
-- deriving.  The example given for @DBType@ added a @UserId@ @newtype@, but
-- without a @DBEq@ instance won't actually be able to use that in joins or
-- where-clauses, because it lacks equality. We can add this by changing our
-- @newtype@ definition to:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (DBType, DBEq)
-- 
-- This will re-use the equality logic for @Int32@, which is to just use the
-- @=@ operator.
-- 
-- [ @DBEq@ with @DeriveAnyType@ ]
-- 
-- You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
-- equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
-- values. Extending the example from 'Rel8.ReadShow''s 'Rel8.DBType' instance,
-- we could add equality to @Color@ by writing:
-- 
-- >>> :{
-- data Color = Red | Green | Blue | Purple | Gold
--   deriving (Generic, Show, Read, DBEq)
--   deriving DBType via ReadShow Color
-- :}
-- 
-- This means @Color@s will be treated as the literal strings @"Red"@,
-- @"Green"@, etc, in the database, and they can be compared for equality by
-- just using @=@.
class DBType a => DBEq (a :: Type) where
  eqExprs :: Expr a -> Expr a -> Expr Bool
  eqExprs = binExpr (Opaleye.:==)


instance DBEq Scientific


instance DBEq Int16


instance DBEq Int32


instance DBEq Int64


instance DBEq Double


instance DBEq Text


instance DBEq Bool


instance DBEq UTCTime


instance DBEq Day


instance DBEq (CI Text)


instance DBEq (CI Data.Text.Lazy.Text)


