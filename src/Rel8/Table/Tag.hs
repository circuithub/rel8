{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Tag
  ( Tag(..), Taggable
  , fromExpr
  , fromName
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Type.Monoid ( DBMonoid )


type Tag :: Symbol -> Type -> Type
data Tag label a = Tag
  { expr :: Expr a
  , name :: Name a
  }


type Taggable :: Type -> Constraint
class Taggable a where
  tappend :: KnownSymbol label => Tag label a -> Tag label a -> Tag label a
  tempty :: KnownSymbol label => Tag label a


instance Sql DBMonoid a => Taggable a where
  tappend :: forall label. KnownSymbol label
    => Tag label a -> Tag label a -> Tag label a
  tappend a b = Tag
    { expr = expr a <> expr b
    , name = case (name a, symbolVal (Proxy @label)) of
        (Name x, y)
          | x == y -> name b
          | otherwise -> name a
    }
  {-# INLINABLE tappend #-}

  tempty :: forall label. KnownSymbol label => Tag label a
  tempty = Tag
    { expr = mempty
    , name = Name (symbolVal (Proxy @label))
    }
  {-# INLINABLE tempty #-}


instance (KnownSymbol label, Taggable a) => Semigroup (Tag label a) where
  (<>) = tappend


instance (KnownSymbol label, Taggable a) => Monoid (Tag label a) where
  mempty = tempty


fromExpr :: forall label a. (KnownSymbol label, Taggable a)
  => Expr a -> Tag label a
fromExpr expr = (tempty @a @label) {expr}


fromName :: forall a label. Taggable a => Name a -> Tag label a
fromName name = (tempty @a @"") {name}
