{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Rewrite ( Rewrite(..), rewriteExpr ) where

import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.Top


-- | @Rewrite f g a b@ says that @a@ can be rewritten into @b@ by changing the
-- underlying data functor from @f@ to @g@. For higher-kinded data types, this
-- is akin to a map operation @t f -> t g@, but this class specifically doesn't
-- talk about higher-kinded data in order to be more general.
class Rewrite f g a b | f g a -> b, a -> f, b -> g, f g b -> a where
  rewrite :: ( forall x. C f x -> C g x ) -> a -> b


instance ( HigherKinded t, f ~ u, g ~ v, ZipRecord t u v Top ) => Rewrite f g ( t u ) ( t v ) where
  rewrite f t =
    runIdentity ( zipRecord (Proxy @Top) ( \_ -> pure . f ) t t )


instance a ~ b => Rewrite ( Expr m ) ( Expr n ) ( Expr m a ) ( Expr n b ) where
  rewrite f t =
    toColumn ( f ( C t ) )


instance Rewrite f g a b => Rewrite f g ( Sum a ) ( Sum b ) where
  rewrite f ( Sum a ) =
    Sum ( rewrite f a )


rewriteExpr :: forall m n a b. Rewrite ( Expr m ) ( Expr n ) a b => a -> b
rewriteExpr = rewrite \( C x ) -> C ( Expr ( toPrimExpr x ) )
