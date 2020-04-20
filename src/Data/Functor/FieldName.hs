{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Data.Functor.FieldName ( FieldName( FieldName ), unFieldName ) where

-- adjunctions
import Data.Functor.Rep ( Rep, Representable, apRep, index, pureRep, tabulate )

-- base
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Kind ( Type )
import GHC.TypeLits ( Symbol )

-- distributive
import Data.Distributive ( Distributive, distribute )

-- record-hasfield
import GHC.Records.Extra ( HasField, hasField )

-- rel8
import Data.Tagged.PolyKinded ( Tagged( Tagged ) )


-- | Tag a value with the name of the field it belongs to.
--
-- This is very similar to using 'Data.Tagged.Tagged', but has an alternative
-- representation (@FieldName name () -> a@) that carries slightly more
-- information.
newtype FieldName (name :: Symbol) (a :: Type) =
  FieldName { unFieldName :: a }
  deriving (Functor, Foldable) via Tagged name
  deriving (Semigroup) via Tagged name a


instance Applicative ( FieldName name ) where
  pure =
    pureRep

  (<*>) =
    apRep


instance Traversable (FieldName name) where
  traverse f =
    fmap FieldName . f . unFieldName


instance Representable (FieldName name) where
  type Rep (FieldName name) =
    FieldName name ()

  index (FieldName name) _ =
    name

  tabulate f =
    FieldName (f (FieldName ()))


instance Distributive (FieldName name) where
  distribute =
    FieldName . fmap unFieldName


instance (name ~ name', f ~ g) => HasField name (Compose (FieldName name') f i) (g i) where
  hasField (Compose (FieldName x)) = (setter, getter) where
    setter = Compose . FieldName
    getter = x
