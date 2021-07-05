{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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
  , aggregateEitherTable
  , nameEitherTable
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Category ( id )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Type )
import Data.Type.Equality ( apply )
import Prelude hiding ( id, undefined )

-- rel8
import Rel8.Aggregate ( Col( A ), Aggregate )
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Expr.Aggregate ( groupByExpr )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Context.Nullify ( Nullifiable, nullifier, unnullifier )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.Either ( HEitherTable(..) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( hnullify, hunnullify )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col( N ), Name )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , reify, unreify, coherence, congruence
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
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
  { tag :: Col context ('Spec EitherTag)
  , left :: a
  , right :: b
  }
  deriving stock Functor


instance Bifunctor (EitherTable context) where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance (context ~ Expr, Table Expr a) => Apply (EitherTable context a) where
  EitherTable (E tag) l1 f <.> EitherTable (E tag') l2 a =
    EitherTable (E (tag <> tag')) (bool l1 l2 (isLeft tag)) (f a)


instance (context ~ Expr, Table Expr a) => Applicative (EitherTable context a) where
  pure = rightTable
  (<*>) = (<.>)


instance (context ~ Expr, Table Expr a) => Bind (EitherTable context a) where
  EitherTable (E tag) l1 a >>- f = case f a of
    EitherTable (E tag') l2 b ->
      EitherTable (E (tag <> tag')) (bool l1 l2 (isRight tag)) b


instance (context ~ Expr, Table Expr a) => Monad (EitherTable context a) where
  (>>=) = (>>-)


instance (context ~ Expr, Table Expr a, Table Expr b) =>
  Semigroup (EitherTable context a b)
 where
  a <> b = bool a b (isRightTable a)


instance
  ( Table context a, Table context b
  , Nullifiable context, context ~ context'
  )
  => Table context' (EitherTable context a b)
 where
  type Columns (EitherTable context a b) = HEitherTable (Columns a) (Columns b)
  type Context (EitherTable context a b) = Context a

  toColumns EitherTable {tag, left, right} = HEitherTable
    { htag = hlabel (HType tag)
    , hleft = hlabel $ hnullify (nullifier tag isLeft) $ toColumns left
    , hright = hlabel $ hnullify (nullifier tag isRight) $ toColumns right
    }

  fromColumns HEitherTable {htag, hleft, hright} = EitherTable
    { tag = unHIdentity (hunlabel htag)
    , left =
        fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel
        hleft
    , right =
        fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel
        hright
    }

  reify = liftA2 bimap reify reify
  unreify = liftA2 bimap unreify unreify

  coherence = coherence @context @a
  congruence proof abstract =
    id `apply`
    congruence @context @a proof abstract `apply`
    congruence @context @b proof abstract


instance
  ( Nullifiable from, from ~ from'
  , Nullifiable to, to ~ to'
  , Recontextualize from to a1 b1
  , Recontextualize from to a2 b2
  )
  => Recontextualize from to (EitherTable from' a1 a2) (EitherTable to' b1 b2)


instance (EqTable a, EqTable b, context ~ Expr) =>
  EqTable (EitherTable context a b)
 where
  eqTable = HEitherTable
    { htag = hlabel (HType Dict)
    , hleft = hlabel (hnullify (\_ Dict -> Dict) (eqTable @a))
    , hright = hlabel (hnullify (\_ Dict -> Dict) (eqTable @b))
    }


instance (OrdTable a, OrdTable b, context ~ Expr) =>
  OrdTable (EitherTable context a b)
 where
  ordTable = HEitherTable
    { htag = hlabel (HType Dict)
    , hleft = hlabel (hnullify (\_ Dict -> Dict) (ordTable @a))
    , hright = hlabel (hnullify (\_ Dict -> Dict) (ordTable @b))
    }


type instance FromExprs (EitherTable _context a b) =
  Either (FromExprs a) (FromExprs b)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ EitherTable Expr exprs1 exprs2) =>
  ToExprs x (Either a b)
 where
  fromResult =
    bimap (fromResult @exprs1) (fromResult @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult @exprs1) (toResult @exprs2)


-- | Test if an 'EitherTable' is a 'leftTable'.
isLeftTable :: EitherTable Expr a b -> Expr Bool
isLeftTable EitherTable {tag = E tag} = isLeft tag


-- | Test if an 'EitherTable' is a 'rightTable'.
isRightTable :: EitherTable Expr a b -> Expr Bool
isRightTable EitherTable {tag = E tag} = isRight tag


-- | Pattern match/eliminate an 'EitherTable', by providing mappings from a
-- 'leftTable' and 'rightTable'.
eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable Expr a b -> c
eitherTable f g EitherTable {tag = E tag, left, right} =
  bool (f left) (g right) (isRight tag)


-- | Construct a left 'EitherTable'. Like 'Left'.
leftTable :: Table Expr b => a -> EitherTable Expr a b
leftTable a = EitherTable (E (litExpr IsLeft)) a undefined


-- | Construct a right 'EitherTable'. Like 'Right'.
rightTable :: Table Expr a => b -> EitherTable Expr a b
rightTable = EitherTable (E (litExpr IsRight)) undefined


-- | Lift a pair of aggregating functions to operate on an 'EitherTable'.
-- @leftTable@s and @rightTable@s are grouped separately.
aggregateEitherTable :: ()
  => (exprs -> aggregates)
  -> (exprs' -> aggregates')
  -> EitherTable Expr exprs exprs'
  -> EitherTable Aggregate aggregates aggregates'
aggregateEitherTable f g (EitherTable (E tag) a b) =
  EitherTable (A (groupByExpr tag)) (f a) (g b)


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
nameEitherTable = EitherTable . N
