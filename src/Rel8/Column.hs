{-# language TypeFamilies #-}

module Rel8.Column ( Column, C(..) ) where

import Data.Functor.Identity
import Data.Kind


{-| The @Column@ type family should be used to indicate which fields of your
data types are single columns in queries. This type family has special
support when a query is executed, allowing you to use a single data type for
both query data and rows decoded to Haskell.

To understand why this type family is special, let's consider a simple
higher-kinded data type of Haskell packages:

@
data HaskellPackage f =
  HaskellPackage
    { packageName :: Column f String
    , packageAuthor :: Column f String
    }
@

In queries, @f@ will be some type of 'Rel8.Expr.Expr', and @Column ( Expr .. )@
reduces to just @Expr ..@:

>>> :t packageName ( package :: Package ( Expr m ) )
Expr m String

When we 'Rel8.Query.select' queries of this type, @f@ will be instantiated as
@Identity@, at which point all wrapping entire disappears:

>>> :t packageName ( package :: Package Identity )
String

In @rel8@ we try hard to always know roughly what @f@ is, which means typed
holes should mention precise types, rather than the @Column@ type family. You
should only need to be aware of the type family when defining your table types.
-}
type family Column ( f :: Type -> Type ) ( a :: Type ) :: Type where
  Column Identity a = a
  Column f a = f a


-- | The @C@ newtype simply wraps 'Column', but this allows us to work
-- injectivity problems of functions that return type family applications
-- (for example, 'Rel8.HigherKinded.zipRecord').
newtype C f x =
  C { toColumn :: Column f x }
