{-# language DataKinds #-}
{-# language FlexibleContexts #-}
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
  , fromIdentity
  , fromName
  )
where

-- base
import Control.Applicative ( (<|>), empty, liftA2 )
import Data.Kind ( Type )
import Data.Foldable ( fold )
import Data.Monoid ( getFirst )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate, Aggregator, foldInputs )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Nullability
  ( Nullability( Nullable, NonNullable )
  , Sql, nullabilization, mapSql
  )
import Rel8.Type ( DBType )
import Rel8.Type.Monoid ( DBMonoid )
import Rel8.Type.Semigroup ( DBSemigroup )


type Tag :: Symbol -> Type -> Type
data Tag label a = Tag
  { expr :: Expr a
  , aggregator :: Maybe Aggregator
  , identity :: a
  , name :: Name a
  }


class Taggable a where
  tappend :: KnownSymbol label => Tag label a -> Tag label a -> Tag label a
  tempty :: KnownSymbol label => Tag label a
  tlit :: a -> Expr a


instance (Sql Monoid a, Sql DBMonoid a) => Taggable a where
  tappend :: forall label. KnownSymbol label
    => Tag label a -> Tag label a -> Tag label a
  tappend a b = Tag
    { expr = case mapSql dbSemigroupFromDBMonoid (Dict @_ @a) of
        Dict -> expr a <> expr b
    , aggregator = aggregator a <|> aggregator b
    , identity = case mapSql semigroupFromMonoid (Dict @_ @a) of
        Dict -> case nullabilization @a of
          Nullable -> liftA2 (<>) (identity a) (identity b)
          NonNullable -> identity a <> identity b
    , name = case (name a, symbolVal (Proxy @label)) of
        (Name x, y)
          | x == y -> name b
          | otherwise -> name a
    }

  tempty :: forall label. KnownSymbol label => Tag label a
  tempty = Tag
    { expr = mempty
    , aggregator = empty
    , identity = case nullabilization @a of
        Nullable -> Just mempty
        NonNullable -> mempty
    , name = Name (symbolVal (Proxy @label))
    }

  tlit = case mapSql (dbTypeFromDBSemigroup . dbSemigroupFromDBMonoid) dict of
    Dict -> litExpr
    where
      dict = Dict @_ @a


instance (KnownSymbol label, Taggable a) => Semigroup (Tag label a) where
  (<>) = tappend


instance (KnownSymbol label, Taggable a) => Monoid (Tag label a) where
  mempty = tempty


fromAggregate :: forall a label. (KnownSymbol label, Taggable a)
  => Aggregate (Expr a) -> Tag label a
fromAggregate = fold . getFirst . foldInputs go
  where
    go aggregator primExpr = pure $ (tempty @a @label)
      { expr = fromPrimExpr primExpr
      , aggregator
      }


fromExpr :: forall label a. (KnownSymbol label, Taggable a)
  => Expr a -> Tag label a
fromExpr expr = (tempty @a @label) {expr}


fromIdentity :: forall a label. (KnownSymbol label, Taggable a)
  => a -> Tag label a
fromIdentity identity = (mempty @(Tag label a))
  { expr = tlit identity
  , identity
  }


fromName :: forall a label. Taggable a => Name a -> Tag label a
fromName name = (mempty @(Tag "" a)) {name}


dbTypeFromDBSemigroup :: Dict DBSemigroup a -> Dict DBType a
dbTypeFromDBSemigroup Dict = Dict


dbSemigroupFromDBMonoid :: Dict DBMonoid a -> Dict DBSemigroup a
dbSemigroupFromDBMonoid Dict = Dict


semigroupFromMonoid :: Dict Monoid a -> Dict Semigroup a
semigroupFromMonoid Dict = Dict
