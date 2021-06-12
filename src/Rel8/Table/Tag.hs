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
  , fromAggregate
  , fromExpr
  , fromName
  )
where

-- base
import Control.Applicative ( (<|>), empty )
import Data.Kind ( Constraint, Type )
import Data.Foldable ( fold )
import Data.Monoid ( getFirst )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregator, foldInputs )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Type.Monoid ( DBMonoid )


type Tag :: Symbol -> Type -> Type
data Tag label a = Tag
  { expr :: Expr a
  , aggregator :: Maybe Aggregator
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
    , aggregator = aggregator a <|> aggregator b
    , name = case (name a, symbolVal (Proxy @label)) of
        (Name x, y)
          | x == y -> name b
          | otherwise -> name a
    }
  {-# INLINABLE tappend #-}

  tempty :: forall label. KnownSymbol label => Tag label a
  tempty = Tag
    { expr = mempty
    , aggregator = empty
    , name = Name (symbolVal (Proxy @label))
    }
  {-# INLINABLE tempty #-}


instance (KnownSymbol label, Taggable a) => Semigroup (Tag label a) where
  (<>) = tappend


instance (KnownSymbol label, Taggable a) => Monoid (Tag label a) where
  mempty = tempty


fromAggregate :: forall a label. (KnownSymbol label, Taggable a)
  => Aggregate a -> Tag label a
fromAggregate = fold . getFirst . foldInputs go
  where
    go aggregator primExpr = pure $ (tempty @a @label)
      { expr = fromPrimExpr primExpr
      , aggregator
      }


fromExpr :: forall label a. (KnownSymbol label, Taggable a)
  => Expr a -> Tag label a
fromExpr expr = (tempty @a @label) {expr}


fromName :: forall a label. Taggable a => Name a -> Tag label a
fromName name = (tempty @a @"") {name}
