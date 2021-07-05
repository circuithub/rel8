{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Type.Composite
  ( Composite( Composite )
  , DBComposite( compositeFields, compositeTypeName )
  , compose, decompose
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Col( E ), Expr )
import Rel8.Expr.Opaleye ( castExpr, fromPrimExpr, toPrimExpr )
import Rel8.Schema.HTable ( HTable, hfield, hspecs, htabulate, htabulateA )
import Rel8.Schema.Name ( Col( N ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( Col( R ), Result )
import Rel8.Schema.Spec ( SSpec( SSpec, nullity, info ) )
import Rel8.Table ( fromColumns, toColumns, fromResult, toResult )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.HKD ( HKD, HKDable )
import Rel8.Table.Ord ( OrdTable )
import Rel8.Table.Rel8able ()
import Rel8.Table.Serialize ( lit )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Ord ( DBOrd, DBMax, DBMin )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )


-- | A deriving-via helper type for column types that store a Haskell product
-- type in a single Postgres column using a Postgres composite type.
--
-- Note that this must map to a specific extant type in your database's schema
-- (created with @CREATE TYPE@). Use 'DBComposite' to specify the name of this
-- Postgres type and the names of the individual fields (for projecting with
-- 'decompose').
type Composite :: Type -> Type
newtype Composite a = Composite
  { unComposite :: a
  }


instance DBComposite a => DBType (Composite a) where
  typeInformation = TypeInformation
    { decode = Hasql.composite (Composite . fromResult @_ @(HKD a Expr) <$> decoder)
    , encode = encoder . lit . toResult @_ @(HKD a Expr) . unComposite
    , typeName = compositeTypeName @a
    }


instance (DBComposite a, EqTable (HKD a Expr)) => DBEq (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBOrd (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBMax (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBMin (Composite a)


-- | 'DBComposite' is used to associate composite type metadata with a Haskell
-- type.
type DBComposite :: Type -> Constraint
class (DBType a, HKDable a) => DBComposite a where
  -- | The names of all fields in the composite type that @a@ maps to.
  compositeFields :: HKD a Name

  -- | The name of the composite type that @a@ maps to.
  compositeTypeName :: String


-- | Collapse a 'HKD' into a PostgreSQL composite type.
--
-- 'HKD' values are represented in queries by having a column for each field in
-- the corresponding Haskell type. 'compose' collapses these columns into a
-- single column expression, by combining them into a PostgreSQL composite
-- type.
compose :: DBComposite a => HKD a Expr -> Expr a
compose = castExpr . fromPrimExpr . encoder . toColumns


-- | Expand a composite type into a 'HKD'.
--
-- 'decompose' is the inverse of 'compose'.
decompose :: forall a. DBComposite a => Expr a -> HKD a Expr
decompose (toPrimExpr -> a) = fromColumns $ htabulate \field ->
  case hfield names field of
    N (Name name) -> case hfield hspecs field of
      SSpec {} -> E $ fromPrimExpr $ Opaleye.CompositeExpr a name
  where
    names = toColumns (compositeFields @a)


decoder :: HTable t => Hasql.Composite (t (Col Result))
decoder = unwrapApplicative $ htabulateA \field ->
  case hfield hspecs field of
    SSpec {nullity, info} -> WrapApplicative $ R <$>
      case nullity of
        Null -> Hasql.field $ Hasql.nullable $ decode info
        NotNull -> Hasql.field $ Hasql.nonNullable $ decode info


encoder :: HTable t => t (Col Expr) -> Opaleye.PrimExpr
encoder a = Opaleye.FunExpr "ROW" exprs
  where
    exprs = getConst $ htabulateA \field -> case hfield a field of
      E (toPrimExpr -> expr) -> Const [expr]
