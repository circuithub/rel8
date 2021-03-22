{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options_ghc -Wno-orphans #-}

module Rel8.Table.EitherTable
  ( EitherTable(..)
  , EitherTag(..)
  , eitherTable
  , leftTable
  , rightTable
  , isLeftTable
  , isRightTable
  ) where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import qualified Data.Bool
import Data.Kind ( Type )
import Data.Semigroup ( Min( Min ) )

-- rel8
import Rel8.Context ( Column( I ), Context( Column ), Defaulting( NoDefault ), Meta( Meta ), unI )
import Rel8.DBType ( DBType, typeInformation )
import Rel8.DBType.DBEq ( DBEq, (==.) )
import Rel8.DBType.DBMonoid ( DBMonoid, memptyExpr )
import Rel8.DBType.DBOrd ( DBOrd )
import Rel8.DBType.DBSemigroup ( DBSemigroup, (<>.) )
import Rel8.DatabaseType ( mapDatabaseType )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.), ifThenElse_ )
import Rel8.Expr.Instances ( Column( ExprColumn ), fromExprColumn )
import Rel8.Expr.Opaleye ( unsafeCoerceExpr, unsafeNullExpr )
import Rel8.HTable ( ColType, HAllColumns, HField, HTable, hdbtype, hfield, hmap, htabulate, htabulateMeta )
import Rel8.HTable.HIdentity ( HIdentity( HIdentity ) )
import Rel8.HTable.HMapTable
  ( HMapTable( HMapTable )
  , HMapTableField( HMapTableField )
  , Precompose( Precompose )
  )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.Info ( Column( InfoColumn ), HasInfo, Info( Null, NotNull ) )
import Rel8.Serializable ( ExprFor, Serializable, lit, pack, unpack )
import Rel8.Table ( Columns, Table, fromColumns, nullTable, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.MaybeTable ( MakeNull )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


type EitherTable :: Type -> Type -> Type


data EitherTable a b = EitherTable
  { tag :: Expr EitherTag
  , left :: a
  , right :: b
  }
  deriving stock (Functor)


instance Bifunctor EitherTable where
  bimap f g (EitherTable tag a b) = EitherTable tag (f a) (g b)


instance Table Expr a => Apply (EitherTable a) where
  EitherTable tag l1 f <.> EitherTable tag' l2 a =
    EitherTable (tag <> tag') (bool l1 l2 (isLeft tag)) (f a)


instance Table Expr a => Applicative (EitherTable a) where
  pure = rightTable
  (<*>) = (<.>)


instance Table Expr a => Bind (EitherTable a) where
  EitherTable tag l1 a >>- f = case f a of
    EitherTable tag' l2 b ->
      EitherTable (tag <> tag') (bool l1 l2 (isRight tag)) b


instance Table Expr a => Monad (EitherTable a) where
  (>>=) = (>>-)


instance (Table Expr (EitherTable a b), Table Expr a, Table Expr b) => Semigroup (EitherTable a b) where
  a <> b = bool a b (isRightTable a)


instance (HAllColumns (Columns (EitherTable a b)) (ColType HasInfo), Table Expr a, Table Expr b) => Table Expr (EitherTable a b) where
  type Columns (EitherTable a b) =
    HPair
      (HIdentity ('Meta 'NoDefault EitherTag))
      (HPair (HMapTable MakeNull (Columns a)) (HMapTable MakeNull (Columns b)))

  toColumns (EitherTable tag l r) =
    HPair (HIdentity $ ExprColumn tag) $
      HPair
        (HMapTable (htabulateMeta (f (tag ==. lit IsLeft) (toColumns l))))
        (HMapTable (htabulateMeta (f (tag ==. lit IsRight) (toColumns r))))
    where
      f :: forall cols d x. HTable cols => Expr Bool -> cols (Column Expr) -> HField cols ('Meta d x) -> Precompose MakeNull (Column Expr) ('Meta d x)
      f p cols i = Precompose
        case hfield hdbtype i of
          InfoColumn (NotNull _) ->
            ExprColumn $
            ifThenElse_
              p
              (unsafeCoerceExpr (fromExprColumn (hfield cols i)))
              unsafeNullExpr

          InfoColumn (Null _) ->
            ExprColumn $
            ifThenElse_
              p
              (fromExprColumn (hfield cols i))
              unsafeNullExpr

  fromColumns (HPair (HIdentity tag) (HPair (HMapTable l) (HMapTable r))) =
    EitherTable
      (fromExprColumn tag)
      (fromColumns (hmap (\(Precompose e) -> ExprColumn (unsafeCoerceExpr (fromExprColumn e))) l))
      (fromColumns (hmap (\(Precompose e) -> ExprColumn (unsafeCoerceExpr (fromExprColumn e))) r))


instance (Table Expr (EitherTable d e), a ~ EitherTable d e, Serializable d b, Serializable e c) => ExprFor a (Either b c) where
  pack (HPair (HIdentity (I tag)) (HPair (HMapTable l) (HMapTable r))) =
    case tag of
      IsLeft -> Left $ pack @d $ htabulate \i ->
        case hfield hdbtype i of
          InfoColumn (NotNull _) ->
            case hfield l i of
              Precompose (I Nothing)  -> error "Impossible"
              Precompose (I (Just x)) -> I x

          InfoColumn (Null _) ->
            case hfield l i of
              Precompose (I x) -> I x

      IsRight -> Right $ pack @e $ htabulate \i ->
        case hfield hdbtype i of
          InfoColumn (NotNull _) ->
            case hfield r i of
              Precompose (I Nothing)  -> error "Impossible"
              Precompose (I (Just x)) -> I x

          InfoColumn (Null _) ->
            case hfield r i of
              Precompose (I x) -> I x

  unpack = \case
    Left (unpack @d -> a) -> HPair (HIdentity (I IsLeft)) (HPair x y)
      where
        x = htabulateMeta @(HMapTable MakeNull (Columns d)) \(HMapTableField i) ->
          case hfield hdbtype i of
            InfoColumn (NotNull _) -> I $ Just $ unI $ hfield a i
            InfoColumn (Null _)    -> hfield a i

        y = htabulateMeta @(HMapTable MakeNull (Columns e)) \(HMapTableField i) ->
          case hfield hdbtype i of
            InfoColumn (NotNull _) -> I Nothing
            InfoColumn (Null _)    -> I Nothing

    Right (unpack @e -> a) -> HPair (HIdentity (I IsLeft)) (HPair x y)
      where
        x = htabulateMeta @(HMapTable MakeNull (Columns d)) \(HMapTableField i) ->
          case hfield hdbtype i of
            InfoColumn (NotNull _) -> I Nothing
            InfoColumn (Null _)    -> I Nothing

        y = htabulateMeta @(HMapTable MakeNull (Columns e)) \(HMapTableField i) ->
          case hfield hdbtype i of
            InfoColumn (NotNull _) -> I $ Just $ unI $ hfield a i
            InfoColumn (Null _)    -> hfield a i


isLeftTable :: EitherTable a b -> Expr Bool
isLeftTable = isLeft . tag


isRightTable :: EitherTable a b -> Expr Bool
isRightTable = isRight . tag


eitherTable :: Table Expr c
  => (a -> c) -> (b -> c) -> EitherTable a b -> c
eitherTable f g EitherTable {tag, left, right} =
  bool (f left) (g right) (isRight tag)


leftTable :: Table Expr b => a -> EitherTable a b
leftTable a = EitherTable (lit IsLeft) a nullTable


rightTable :: Table Expr a => b -> EitherTable a b
rightTable = EitherTable (lit IsRight) nullTable


data EitherTag = IsLeft | IsRight
  deriving stock (Eq, Ord, Read, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via (Min EitherTag)
  deriving anyclass (DBEq, DBOrd)


instance DBType EitherTag where
  typeInformation = mapDatabaseType to from typeInformation
    where
      to = Data.Bool.bool IsLeft IsRight
      from IsLeft = False
      from IsRight = True


instance DBSemigroup EitherTag where
  a <>. b = unsafeCoerceExpr $ unsafeCoerceExpr a &&. unsafeCoerceExpr b


instance DBMonoid EitherTag where
  memptyExpr = lit mempty


isLeft :: Expr EitherTag -> Expr Bool
isLeft = (lit IsLeft ==.)


isRight :: Expr EitherTag -> Expr Bool
isRight = (lit IsLeft ==.)
