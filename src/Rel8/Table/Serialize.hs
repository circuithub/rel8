{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, parse
  , ToExprs(..), FromExprs
  , TToExprs
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table
  ( GGToExprs, GGColumns, ggfromResult, ggtoResult
  , GAlgebra
  )
import Rel8.Kind.Algebra ( KnownAlgebra )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ) )
import Rel8.Schema.Null ( NotNull, Sql )
import Rel8.Schema.Result ( Col( Result ), Result )
import Rel8.Schema.Spec ( SSpec(..), KnownSpec )
import Rel8.Table ( Table, Columns, fromColumns, toColumns, TColumns )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )


type ToExprs :: Type -> Type -> Constraint
class Table Expr exprs => ToExprs exprs a where
  fromResult :: Columns exprs (Col Result) -> a
  toResult :: a -> Columns exprs (Col Result)

  default fromResult ::
    ( Generic (Record a)
    , KnownAlgebra (GAlgebra (Rep (Record exprs)))
    , Eval (GGToExprs (GAlgebra (Rep (Record exprs))) TToExprs TColumns (Rep (Record exprs)) (Rep (Record a)))
    , Columns exprs ~ Eval (GGColumns (GAlgebra (Rep (Record exprs))) TColumns (Rep (Record exprs)))
    )
    => Columns exprs (Col Result) -> a
  fromResult =
    unrecord .
    to .
    ggfromResult
      @(GAlgebra (Rep (Record exprs)))
      @TToExprs
      @TColumns
      @(Rep (Record exprs))
      (\(_ :: proxy expr) -> fromResult @expr)

  default toResult ::
    ( Generic (Record a)
    , KnownAlgebra (GAlgebra (Rep (Record exprs)))
    , Eval (GGToExprs (GAlgebra (Rep (Record exprs))) TToExprs TColumns (Rep (Record exprs)) (Rep (Record a)))
    , Columns exprs ~ Eval (GGColumns (GAlgebra (Rep (Record exprs))) TColumns (Rep (Record exprs)))
    )
    => a -> Columns exprs (Col Result)
  toResult =
    ggtoResult
      @(GAlgebra (Rep (Record exprs)))
      @TToExprs
      @TColumns
      @(Rep (Record exprs))
      (\(_ :: proxy expr) -> toResult @expr) .
    from .
    Record


data TToExprs :: Type -> Type -> Exp Constraint
type instance Eval (TToExprs exprs a) = ToExprs exprs a


instance {-# OVERLAPPABLE #-} (Sql DBType a, x ~ Expr a) => ToExprs x a where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, x ~ [a]) => ToExprs (Expr x) [a] where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ Maybe a) => ToExprs (Expr x) (Maybe a)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ NonEmpty a) => ToExprs (Expr x) (NonEmpty a)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ (exprs1, exprs2)) =>
  ToExprs x (a, b)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , x ~ (exprs1, exprs2, exprs3)
  )
  => ToExprs x (a, b, c)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  )
  => ToExprs x (a, b, c, d)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  )
  => ToExprs x (a, b, c, d, e)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6)
  )
  => ToExprs x (a, b, c, d, e, f)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , ToExprs exprs7 g
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6, exprs7)
  )
  => ToExprs x (a, b, c, d, e, f, g)


instance (HTable t, result ~ Col Result, x ~ t (Col Expr)) =>
  ToExprs x (t result)
 where
  fromResult = id
  toResult = id


instance (KnownSpec spec, x ~ Col Expr spec) =>
  ToExprs x (Col Result spec)
 where
  fromResult = fromColumns
  toResult = toColumns


type FromExprs :: Type -> Type
type family FromExprs a
type instance FromExprs (Expr a) = a
type instance FromExprs (Col Expr spec) = Col Result spec
type instance FromExprs (a, b) = (FromExprs a, FromExprs b)
type instance FromExprs (a, b, c) = (FromExprs a, FromExprs b, FromExprs c)
type instance FromExprs (a, b, c, d) =
  (FromExprs a, FromExprs b, FromExprs c, FromExprs d)
type instance FromExprs (a, b, c, d, e) =
  (FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e)
type instance FromExprs (a, b, c, d, e, f) =
  ( FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e
  , FromExprs f
  )
type instance FromExprs (a, b, c, d, e, f, g) =
  ( FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e
  , FromExprs f, FromExprs g
  )
type instance FromExprs (t (Col Expr)) = t (Col Result)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
type Serializable :: Type -> Type -> Constraint
class (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a
instance {-# OVERLAPPING #-} Sql DBType a => Serializable (Expr a) a


-- | Use @lit@ to turn literal Haskell values into expressions. @lit@ is
-- capable of lifting single @Expr@s to full tables.
lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litHTable . toResult @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResult @exprs <$> parseHTable


litHTable :: HTable t => t (Col Result) -> t (Col Expr)
litHTable as = htabulate $ \field ->
  case hfield hspecs field of
    SSpec {nullity, info} -> case hfield as field of
      Result value -> DB (slitExpr nullity info value)


parseHTable :: HTable t => Hasql.Row (t (Col Result))
parseHTable = unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    SSpec {nullity, info} -> Result <$> sparseValue nullity info
