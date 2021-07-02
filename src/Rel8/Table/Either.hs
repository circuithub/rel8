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
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.Either
  ( EitherTable(..)
  , eitherTable, leftTable, rightTable
  , isLeftTable, isRightTable
  , nameEitherTable
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Context.Nullify
  ( Nullifiable
  , encodeTag, decodeTag
  , nullifier, unnullifier
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Table.Tag ( Tag(..), fromExpr, fromName )
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
type EitherTable :: Type -> Type -> Type
data EitherTable a b = EitherTable
  { tag :: Tag "isRight" EitherTag
  , left :: a
  , right :: b
  }
  deriving stock Functor


instance Bifunctor EitherTable where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance Table Expr a => Apply (EitherTable a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (isLeft (expr tag))) (f a)


instance Table Expr a => Applicative (EitherTable a) where
  pure = rightTable
  (<*>) = (<.>)


instance Table Expr a => Bind (EitherTable a) where
  EitherTable tag l1 a >>- f = case f a of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (isRight (expr tag))) b


instance Table Expr a => Monad (EitherTable a) where
  (>>=) = (>>-)


instance (Table Expr a, Table Expr b) => Semigroup (EitherTable a b) where
  a <> b = bool a b (isRightTable a)


instance
  ( Table context a, Table context b
  , Nullifiable context
  )
  => Table context (EitherTable a b)
 where
  type Columns (EitherTable a b) = HEitherTable (Columns a) (Columns b)
  type Context (EitherTable a b) = Context a

  toColumns EitherTable {tag, left, right} = HEitherTable
    { htag
    , hleft = hlabel $ hnullify (nullifier tag isLeft) $ toColumns left
    , hright = hlabel $ hnullify (nullifier tag isRight) $ toColumns right
    }
    where
      htag = hlabel $ HType $ encodeTag tag

  fromColumns HEitherTable {htag, hleft, hright} = EitherTable
    { tag
    , left =
        fromColumns $
        runIdentity $
        hunnullify (\a -> pure . unnullifier a) $
        hunlabel
        hleft
    , right =
        fromColumns $
        runIdentity $
        hunnullify (\a -> pure . unnullifier a) $
        hunlabel
        hright
    }
    where
      tag = decodeTag $ unHIdentity $ hunlabel htag

  reify = liftA2 bimap reify reify
  unreify = liftA2 bimap unreify unreify


instance
  ( Nullifiable from, Nullifiable to
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  )
  => Recontextualize from to (EitherTable a1 a2) (EitherTable b1 b2)


instance (EqTable a, EqTable b) => EqTable (EitherTable a b) where
  eqTable = HEitherTable
    { htag = hlabel (HType Dict)
    , hleft = hlabel (hnullify (\_ Dict -> Dict) (eqTable @a))
    , hright = hlabel (hnullify (\_ Dict -> Dict) (eqTable @b))
    }


instance (OrdTable a, OrdTable b) => OrdTable (EitherTable a b) where
  ordTable = HEitherTable
    { htag = hlabel (HType Dict)
    , hleft = hlabel (hnullify (\_ Dict -> Dict) (ordTable @a))
    , hright = hlabel (hnullify (\_ Dict -> Dict) (ordTable @b))
    }


type instance FromExprs (EitherTable a b) = Either (FromExprs a) (FromExprs b)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ EitherTable exprs1 exprs2) =>
  ToExprs x (Either a b)
 where
  fromResult =
    bimap (fromResult @exprs1) (fromResult @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult @exprs1) (toResult @exprs2)


-- | Test if an 'EitherTable' is a 'leftTable'.
isLeftTable :: EitherTable a b -> Expr Bool
isLeftTable = isLeft . expr . tag


-- | Test if an 'EitherTable' is a 'rightTable'.
isRightTable :: EitherTable a b -> Expr Bool
isRightTable = isRight . expr . tag


-- | Pattern match/eliminate an 'EitherTable', by providing mappings from a
-- 'leftTable' and 'rightTable'.
eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight (expr tag))


-- | Construct a left 'EitherTable'. Like 'Left'.
leftTable :: Table Expr b => a -> EitherTable a b
leftTable a = EitherTable (fromExpr (litExpr IsLeft)) a undefined


-- | Construct a right 'EitherTable'. Like 'Right'.
rightTable :: Table Expr a => b -> EitherTable a b
rightTable = rightTableWith undefined


rightTableWith :: a -> b -> EitherTable a b
rightTableWith = EitherTable (fromExpr (litExpr IsRight))


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
  -> EitherTable a b
nameEitherTable = EitherTable . fromName
