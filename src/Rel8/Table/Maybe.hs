{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Maybe ( MaybeTable(..), HMaybeTable(..) ) where

import Data.Kind ( Type )
import Rel8.Expr ( Expr )
import Rel8.Table.Bool ( bool )
import Rel8.Type.Tag ( MaybeTag )
import Rel8.Table ( Table(..) )
import Rel8.Table.Alternative ( AltTable(..), AlternativeTable(..) )
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )
import Rel8.HTable.Maybe ( HMaybeTable(..) )
import Rel8.HTable.Identity ( HIdentity(..) )
import Rel8.NullExpr ( NullExpr )


type MaybeTable :: Type -> Type
data MaybeTable a = MaybeTable
  { tag :: NullExpr MaybeTag
  , just :: a
  }
  deriving stock (Functor)


instance Apply MaybeTable where
  MaybeTable tag f <.> MaybeTable tag' a = MaybeTable (tag <> tag') (f a)


instance Applicative MaybeTable where
  (<*>) = (<.>)
  pure = justTable


instance Bind MaybeTable where
  MaybeTable tag a >>- f = case f a of
    MaybeTable tag' b -> MaybeTable (tag <> tag') b


instance Monad MaybeTable where
  (>>=) = (>>-)


instance AltTable MaybeTable where
  ma@(MaybeTable tag a) <|>: MaybeTable tag' b = MaybeTable
    { tag = bool tag tag' condition
    , just = bool a b condition
    }
    where
      condition = isNothingTable ma


instance AlternativeTable MaybeTable where
  emptyTable = nothingTable


instance (Table Expr a, Semigroup a) => Semigroup (MaybeTable a) where
  ma <> mb = maybeTable mb (\a -> maybeTable ma (justTable . (a <>)) mb) ma


instance (Table Expr a, Semigroup a) => Monoid (MaybeTable a) where
  mempty = nothingTable


instance (Table f a, f ~ Expr) => Table f (MaybeTable a) where
  type Columns (MaybeTable a) = HMaybeTable (Columns a)

  toColumns (MaybeTable x y) = HMaybeTable (HIdentity x) (toColumns y)
  fromColumns (HMaybeTable (HIdentity x) y) = MaybeTable x (fromColumns y)


isNothingTable :: MaybeTable a -> Expr Bool
isNothingTable (MaybeTable tag _) = isNull tag


isJustTable :: MaybeTable a -> Expr Bool
isJustTable (MaybeTable tag _) = isNonNull tag


maybeTable :: Table Expr b => b -> (a -> b) -> MaybeTable a -> b
maybeTable b f ma@(MaybeTable _ a) = bool b (f a) (isNothingTable ma)


nothingTable :: Table Expr a => MaybeTable a
nothingTable = MaybeTable null undefined


justTable :: a -> MaybeTable a
justTable = MaybeTable (nullify (litExpr IsJust))
