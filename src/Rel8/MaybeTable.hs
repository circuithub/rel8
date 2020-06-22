{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MaybeTable where

import Data.Functor.Identity ( Identity(..) )
import Data.Proxy ( Proxy(..) )
import GHC.Generics ( Generic )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Column
import Rel8.Expr
import Rel8.Table


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@. Unfortunately, SQL makes it
impossible to distinguish whether or not an outer join matched any rows based
generally on the row contents - if you were to join a row entirely of nulls,
you can't distinguish if you matched an all null row, or if the match failed.
For this reason @MaybeTable@ contains an extra field - 'nullTag' - to
track whether or not the outer join produced any rows.

-}
data MaybeTable t where
  MaybeTable
    :: { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         nullTag :: Expr ( Maybe Bool )
       , table :: t
       }
    -> MaybeTable t
  deriving
    ( Functor )


instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just True))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftNull (or_ t t')) (f a)
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftNull (or_ t t')) b
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x


data HMaybeTable g f =
  HMaybeTable
    { hnullTag :: Column f (Maybe Bool)
    , hcontents :: g f
    }
  deriving
    (Generic)


deriving instance (forall f. Table (g f)) => HigherKindedTable (HMaybeTable g)


instance (ExprTable a, HigherKindedTable (Structure a)) => Table (MaybeTable a) where
  type Structure (MaybeTable a) = HMaybeTable (Structure a)
  type Context (MaybeTable a) = Expr

  toStructure (MaybeTable x y) = HMaybeTable x (toStructure y)
  fromStructure (HMaybeTable x y) = MaybeTable x (fromStructure y)


maybeTable
  :: ExprTable b
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


noTable :: ExprTable a => MaybeTable a
noTable = MaybeTable tag t
  where
    tag = lit Nothing
    t = fromStructure $ runIdentity $ htabulate (Proxy @DBType) f
      where
        f :: forall a i. DBType a => i a -> Identity (C Expr a)
        f _ = pure $ MkC $ unsafeCoerceExpr $ lit (Nothing @a)


-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@. Like field selection.
--
-- It may be helpful to remember this operator by the mneumonic - '$' on the left
-- means function on the left, '?' on the right means 'MaybeTable' on the right.
infixl 4 $?
($?) :: forall a b. DBType b
  => (a -> Expr b) -> MaybeTable a -> Expr (Maybe b)
f $? ma = maybeTable null_ (unsafeCoerceExpr . f) ma
  where
    null_ =
      unsafeCastExpr
        (typeName (typeInformation @b))
        (fromPrimExpr (Opaleye.ConstExpr Opaleye.NullLit))
