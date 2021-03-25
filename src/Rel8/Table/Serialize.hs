{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, parse
  )
where

-- base
import Data.Bifunctor ( bimap )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr, Encodes, Col(..) )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Identity
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType(..) )
import Rel8.Schema.Nullability ( NotNull, Sql )
import Rel8.Schema.Spec ( SSpec(..), KnownSpec )
import Rel8.Table ( Table, Columns, fromColumns, toColumns )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.These ( TheseTable )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )

-- these
import Data.These ( These )


type IsPlainColumn :: Type -> Bool
type family IsPlainColumn a where
  IsPlainColumn (Either _ _) = 'False
  IsPlainColumn [_] = 'False
  IsPlainColumn (Maybe _) = 'False
  IsPlainColumn (NonEmpty _) = 'False
  IsPlainColumn (These _ _) = 'False
  IsPlainColumn (_, _) = 'False
  IsPlainColumn (_, _, _) = 'False
  IsPlainColumn (_, _, _, _) = 'False
  IsPlainColumn (_, _, _, _, _) = 'False
  IsPlainColumn (_ Identity) = 'False
  IsPlainColumn (_ (Col Identity _)) = 'False
  IsPlainColumn (Identity _) = 'False
  IsPlainColumn _ = 'True


type IsTabular :: Type -> Bool
type family IsTabular a where
  IsTabular (Either _ _) = 'True
  IsTabular [a] = IsListTabular a
  IsTabular (Maybe a) = IsMaybeTabular a
  IsTabular (NonEmpty a) = IsListTabular a
  IsTabular (These _ _) = 'True
  IsTabular (_, _) = 'True
  IsTabular (_, _, _) = 'True
  IsTabular (_, _, _, _) = 'True
  IsTabular (_, _, _, _, _) = 'True
  IsTabular (_ Identity) = 'True
  IsTabular (_ (Col Identity)) = 'True
  IsTabular (Col Identity _) = 'True
  IsTabular _ = 'False


type IsTabular' :: Bool -> Type -> Bool
type family IsTabular' isTabular exprs where
  IsTabular' _ (Expr _) = 'False
  IsTabular' 'False _ = 'False
  IsTabular' _ _ = 'True


type IsMaybeTabular :: Type -> Bool
type family IsMaybeTabular a where
  IsMaybeTabular (Maybe _) = 'True
  IsMaybeTabular a = IsTabular a


type IsListTabular :: Type -> Bool
type family IsListTabular a where
  IsListTabular (Maybe a) = IsMaybeTabular a
  IsListTabular a = IsTabular a


type ToExprs :: Type -> Type -> Constraint
class ExprsFor (IsPlainColumn a) (IsTabular' (IsTabular a) exprs) a exprs => ToExprs a exprs
instance ExprsFor (IsPlainColumn a) (IsTabular' (IsTabular a) exprs) a exprs => ToExprs a exprs


fromIdentity' :: forall exprs a. ToExprs a exprs => Columns exprs (Col Identity) -> a
fromIdentity' = fromIdentity @(IsPlainColumn a) @_ @_ @exprs


toIdentity' :: forall exprs a. ToExprs a exprs => a -> Columns exprs (Col Identity)
toIdentity' = toIdentity @(IsPlainColumn a) @_ @_ @exprs


type ExprsFor :: Bool -> Bool -> Type -> Type -> Constraint
class (Table Expr exprs, isTabular ~ IsTabular a) =>
  ExprsFor isPlainColumn isTabular a exprs where
  fromIdentity :: Columns exprs (Col Identity) -> a
  toIdentity :: a -> Columns exprs (Col Identity)


instance
  ( DBType a
  , NotNull a
  , IsTabular a ~ 'False
  , x ~ Expr a
  )
  => ExprsFor 'True 'False a x
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance
  ( Sql DBType a
  , IsListTabular a ~ 'False
  , x ~ [a]
  ) => ExprsFor 'False 'False [a] (Expr x)
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance
  ( Sql DBType a
  , isTabular ~ 'False
  , NotNull a
  , IsMaybeTabular a ~ 'False
  , x ~ Maybe a
  ) => ExprsFor 'False 'False (Maybe a) (Expr x)
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance
  ( Sql DBType a
  , IsListTabular a ~ 'False
  , x ~ NonEmpty a
  ) => ExprsFor 'False 'False (NonEmpty a) (Expr x)
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False 'False [a] (ListTable exprs)
 where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHListTable
  toIdentity = toHListTable . fmap (toIdentity' @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'False
  ) => ExprsFor 'False 'False (Maybe a) (MaybeTable exprs)
 where
  fromIdentity =
    fmap (fromIdentity' @exprs . hunlabel unlabeler) .
    fromHMaybeTable
  toIdentity =
    toHMaybeTable .
    fmap (hlabel labeler . toIdentity' @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False 'False (NonEmpty a) (NonEmptyTable exprs)
 where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHNonEmptyTable
  toIdentity = toHNonEmptyTable . fmap (toIdentity' @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ EitherTable exprs1 exprs2
  ) => ExprsFor 'False isTabular (Either a b) x
 where
  fromIdentity =
    bimap
      (fromIdentity' @exprs1 . hunlabel unlabeler)
      (fromIdentity' @exprs2 . hunlabel unlabeler) .
    fromHEitherTable
  toIdentity =
    toHEitherTable .
    bimap
      (hlabel labeler . toIdentity' @exprs1)
      (hlabel labeler . toIdentity' @exprs2)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ ListTable exprs
  ) => ExprsFor 'False 'True [a] x
 where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHListTable
  toIdentity = toHListTable . fmap (toIdentity' @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'True
  , x ~ MaybeTable exprs
  ) => ExprsFor 'False 'True (Maybe a) x
 where
  fromIdentity =
    fmap (fromIdentity' @exprs . hunlabel unlabeler) .
    fromHMaybeTable
  toIdentity =
    toHMaybeTable .
    fmap (hlabel labeler . toIdentity' @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ NonEmptyTable exprs
  ) => ExprsFor 'False 'True (NonEmpty a) x
 where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHNonEmptyTable
  toIdentity = toHNonEmptyTable . fmap (toIdentity' @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ TheseTable exprs1 exprs2
  ) => ExprsFor 'False isTabular (These a b) x
 where
  fromIdentity =
    bimap
      (fromIdentity' @exprs1 . hunlabel unlabeler)
      (fromIdentity' @exprs2 . hunlabel unlabeler) .
    fromHTheseTable
  toIdentity =
    toHTheseTable .
    bimap
      (hlabel labeler . toIdentity' @exprs1)
      (hlabel labeler . toIdentity' @exprs2)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2)
  ) => ExprsFor 'False isTabular (a, b) x
 where
  fromIdentity (HPair a b) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    )
  toIdentity (a, b) = HPair
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3)
  ) => ExprsFor 'False isTabular (a, b, c) x
 where
  fromIdentity (HTrio a b c) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    )
  toIdentity (a, b, c) = HTrio
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ExprsFor 'False isTabular (a, b, c, d) x
 where
  fromIdentity (HQuartet a b c d) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    , fromIdentity' @exprs4 $ hunlabel unlabeler d
    )
  toIdentity (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    , hfrt = hlabel labeler $ toIdentity' @exprs4 d
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , ToExprs e exprs5
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  ) => ExprsFor 'False isTabular (a, b, c, d, e) x
 where
  fromIdentity (HQuintet a b c d e) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    , fromIdentity' @exprs4 $ hunlabel unlabeler d
    , fromIdentity' @exprs5 $ hunlabel unlabeler e
    )
  toIdentity (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    , hfrt = hlabel labeler $ toIdentity' @exprs4 d
    , hfft = hlabel labeler $ toIdentity' @exprs5 e
    }


instance
  ( HTable t
  , isTabular ~ 'True
  , result ~ Col Identity
  , x ~ t (Col Expr)
  ) => ExprsFor 'False isTabular (t result) x
 where
  fromIdentity = id
  toIdentity = id


instance
  ( Encodes (t Identity) (t Expr)
  , isTabular ~ 'True
  , result ~ Identity
  , x ~ t Expr
  ) => ExprsFor 'False isTabular (t result) x
 where
  fromIdentity = fromColumns
  toIdentity = toColumns


instance
  ( KnownSpec spec
  , isTabular ~ 'True
  , isSpecial ~ 'True
  , x ~ Col Expr spec
  ) => ExprsFor 'False isTabular (Col Identity spec) x
 where
  fromIdentity = fromColumns
  toIdentity = toColumns


type FromExprs :: Type -> Type
type family FromExprs a where
  FromExprs (Expr a) = a
  FromExprs (Col Expr spec) = Col Identity spec
  FromExprs (EitherTable a b) = Either (FromExprs a) (FromExprs b)
  FromExprs (ListTable a) = [FromExprs a]
  FromExprs (MaybeTable a) = Maybe (FromExprs a)
  FromExprs (NonEmptyTable a) = NonEmpty (FromExprs a)
  FromExprs (TheseTable a b) = These (FromExprs a) (FromExprs b)
  FromExprs (a, b) = (FromExprs a, FromExprs b)
  FromExprs (a, b, c) = (FromExprs a, FromExprs b, FromExprs c)
  FromExprs (a, b, c, d) =
    (FromExprs a, FromExprs b, FromExprs c, FromExprs d)
  FromExprs (a, b, c, d, e) =
    (FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e)
  FromExprs (t Expr) = t Identity
  FromExprs (t (Col Expr)) = t (Col Identity)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
class (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a
instance Serializable (Expr Opaque) Opaque


lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litTable . toIdentity' @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromIdentity' @exprs <$> parseTable


litTable :: Encodes a b => a -> b
litTable (toColumns -> as) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {nullability, info} -> case hfield as field of
      Result value -> DB (slitExpr nullability info value)


parseTable :: Table Identity a => Hasql.Row a
parseTable = fmap fromColumns $ unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    SSpec {nullability, info} -> Result <$> sparseValue nullability info
