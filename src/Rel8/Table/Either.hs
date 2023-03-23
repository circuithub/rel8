{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.Either
  ( EitherTable(..)
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  , aggregateEitherTable
  , nameEitherTable
  )
where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- comonad
import Control.Comonad ( extract )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Kind.Context ( Reifiable )
import Rel8.Schema.Context.Nullify ( Nullifiable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Nullify ( Nullify, aggregateNullify, guard )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Projection ( Biprojectable, Projectable, biproject, project )
import Rel8.Table.Serialize ( ToExprs )
import Rel8.Table.Undefined ( undefined )
import Rel8.Type.Tag ( EitherTag( IsLeft, IsRight ), isLeft, isRight )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


-- | An @EitherTable a b@ is a Rel8 table that contains either the table @a@ or
-- the table @b@. You can construct an @EitherTable@ using 'leftTable' and
-- 'rightTable', and eliminate/pattern match using 'eitherTable'.
--
-- An @EitherTable@ is operationally the same as Haskell's 'Either' type, but
-- adapted to work with Rel8.
type EitherTable :: K.Context -> Type -> Type -> Type
data EitherTable context a b = EitherTable
  { tag :: context EitherTag
  , left :: Nullify context a
  , right :: Nullify context b
  }
  deriving stock Functor


instance Biprojectable (EitherTable context) where
  biproject f g (EitherTable tag a b) =
    EitherTable tag (project f a) (project g b)


instance Nullifiable context => Bifunctor (EitherTable context) where
  bimap f g (EitherTable tag a b) = EitherTable tag (fmap f a) (fmap g b)


instance Projectable (EitherTable context a) where
  project f (EitherTable tag a b) = EitherTable tag a (project f b)


instance (context ~ Expr, Table Expr a) => Apply (EitherTable context a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (isLeft tag)) (f <.> a)


instance (context ~ Expr, Table Expr a) => Applicative (EitherTable context a) where
  pure = rightTable
  (<*>) = (<.>)


instance (context ~ Expr, Table Expr a) => Bind (EitherTable context a) where
  EitherTable tag l1 a >>- f = case f (extract a) of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (isRight tag)) b


instance (context ~ Expr, Table Expr a) => Monad (EitherTable context a) where
  (>>=) = (>>-)


instance (context ~ Expr, Table Expr a, Table Expr b) =>
  Semigroup (EitherTable context a b)
 where
  a <> b = bool a b (isRightTable a)


instance
  ( Table context a, Table context b
  , Reifiable context, context ~ context'
  )
  => Table context' (EitherTable context a b)
 where
  type Columns (EitherTable context a b) = HEitherTable (Columns a) (Columns b)
  type Context (EitherTable context a b) = Context a
  type FromExprs (EitherTable context a b) = Either (FromExprs a) (FromExprs b)
  type Transpose to (EitherTable context a b) =
    EitherTable to (Transpose to a) (Transpose to b)

  toColumns EitherTable {tag, left, right} = HEitherTable
    { htag = hlabel $ HIdentity tag
    , hleft = hlabel $ guard tag (== IsLeft) isLeft $ toColumns left
    , hright = hlabel $ guard tag (== IsRight) isRight $ toColumns right
    }

  fromColumns HEitherTable {htag, hleft, hright} = EitherTable
    { tag = unHIdentity $ hunlabel htag
    , left = fromColumns $ hunlabel hleft
    , right = fromColumns $ hunlabel hright
    }

  toResult = \case
    Left table -> HEitherTable
      { htag = hlabel (HIdentity (Identity IsLeft))
      , hleft = hlabel (toResult @_ @(Nullify context a) (Just table))
      , hright = hlabel (toResult @_ @(Nullify context b) Nothing)
      }
    Right table -> HEitherTable
      { htag = hlabel (HIdentity (Identity IsRight))
      , hleft = hlabel (toResult @_ @(Nullify context a) Nothing)
      , hright = hlabel (toResult @_ @(Nullify context b) (Just table))
      }

  fromResult HEitherTable {htag, hleft, hright} = case hunlabel htag of
    HIdentity (Identity tag) -> case tag of
      IsLeft -> maybe err Left $ fromResult @_ @(Nullify context a) (hunlabel hleft)
      IsRight -> maybe err Right $ fromResult @_ @(Nullify context b) (hunlabel hright)
    where
      err = error "Either.fromColumns: mismatch between tag and data"


instance (EqTable a, EqTable b, context ~ Expr) =>
  EqTable (EitherTable context a b)
 where
  eqTable = HEitherTable
    { htag = hlabel (HIdentity Dict)
    , hleft = hlabel (eqTable @(Nullify context a))
    , hright = hlabel (eqTable @(Nullify context b))
    }


instance (OrdTable a, OrdTable b, context ~ Expr) =>
  OrdTable (EitherTable context a b)
 where
  ordTable = HEitherTable
    { htag = hlabel (HIdentity Dict)
    , hleft = hlabel (ordTable @(Nullify context a))
    , hright = hlabel (ordTable @(Nullify context b))
    }


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ EitherTable Expr exprs1 exprs2) =>
  ToExprs x (Either a b)


-- | Test if an 'EitherTable' is a 'leftTable'.
isLeftTable :: EitherTable Expr a b -> Expr Bool
isLeftTable EitherTable {tag} = isLeft tag


-- | Test if an 'EitherTable' is a 'rightTable'.
isRightTable :: EitherTable Expr a b -> Expr Bool
isRightTable EitherTable {tag} = isRight tag


-- | Pattern match/eliminate an 'EitherTable', by providing mappings from a
-- 'leftTable' and 'rightTable'.
eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable Expr a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f (extract left)) (g (extract right)) (isRight tag)


-- | Construct a left 'EitherTable'. Like 'Left'.
leftTable :: Table Expr b => a -> EitherTable Expr a b
leftTable a = EitherTable (litExpr IsLeft) (pure a) undefined


-- | Construct a right 'EitherTable'. Like 'Right'.
rightTable :: Table Expr a => b -> EitherTable Expr a b
rightTable = EitherTable (litExpr IsRight) undefined . pure


-- | Lift a pair of aggregating functions to operate on an 'EitherTable'.
-- @leftTable@s and @rightTable@s are grouped separately.
aggregateEitherTable :: ()
  => (exprs -> aggregates)
  -> (exprs' -> aggregates')
  -> EitherTable Expr exprs exprs'
  -> EitherTable Aggregate aggregates aggregates'
aggregateEitherTable f g (EitherTable tag a b) = EitherTable
  { tag = groupByExpr tag
  , left = aggregateNullify f a
  , right = aggregateNullify g b
  }


-- | Construct a 'EitherTable' in the 'Name' context. This can be useful if you
-- have a 'EitherTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameEitherTable
  :: Name EitherTag
     -- ^ The name of the column to track whether a row is a 'leftTable' or
     -- 'rightTable'.
  -> a
     -- ^ Names of the columns in the @a@ table.
  -> b
     -- ^ Names of the columns in the @b@ table.
  -> EitherTable Name a b
nameEitherTable tag left right = EitherTable tag (pure left) (pure right)
