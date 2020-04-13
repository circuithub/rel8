{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Data.Functor.FieldName where

import Data.Coerce ( coerce )
import Data.Distributive ( Distributive(..) )
import Data.Functor.Rep ( Representable(..) )
import Data.Kind ( Type )
import GHC.TypeLits ( Symbol )


-- | Tag a value with the name of the field it belongs to.
newtype FieldName (name :: Symbol) (a :: Type) =
  FieldName { unFieldName :: a }
  deriving
    ( Functor )


instance Applicative ( FieldName name ) where
  pure = FieldName
  FieldName f <*> FieldName x = FieldName (f x)


instance Foldable (FieldName name) where
  foldMap f (FieldName x) = f x


instance Traversable (FieldName name) where
  traverse f (FieldName x) = FieldName <$> f x


instance Representable (FieldName name) where
  type Rep (FieldName name) = FieldName name ()
  index (FieldName name) _ = name
  tabulate f = FieldName (f (FieldName ()))


instance Distributive (FieldName name) where
  distribute y = FieldName $ fmap coerce y
