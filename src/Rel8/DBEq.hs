{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.DBEq where

import Data.Int
import Data.Kind
import Data.Text
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Expr
import Rel8.Stuff


{-| Database types that can be compared for equality in queries.

Usually, this means producing an expression using the (overloaded) @=@
operator, but types can provide a more elaborate expression if necessary.

[ @DBEq@ with @newtype@s ]

Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype deriving.
The example given there showed a @UserId@ @newtype@, but we won't actually be
able to use that in joins or where-clauses because it lacks equality. We can
add this by changing our @newtype@ definition to:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving ( DBType, DBEq )
@

This will re-use the equality logic for @Int32@, which is to just use the @=@
operator.

[ @DBEq@ with @DeriveAnyType@ ]

You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
values. Extending the example from 'Rel8.DBType', we could add equality
to @Color@ by writing:

@
data Color = Red | Green | Blue | Purple | Gold
  deriving ( Show, DBType, DBEq )
@

This means @Color@s will be treated as the literal strings @"Red"@, @"Green"@,
etc in the database, and they can be compared for equality by just using @=@.
-}
class DBEq ( a :: Type ) where
  eqExprs :: Expr m a -> Expr m a -> Expr m Bool
  eqExprs =
    binExpr (Opaleye.:==)


instance DBEq String


instance DBEq Int32


instance DBEq Int64


instance DBEq Text


instance ( NotMaybe a, DBEq a ) => DBEq ( Maybe a ) where
  eqExprs a b =
    null_ ( isNull b ) ( \a' -> null_ ( lit False ) ( eqExprs a' ) b ) a
