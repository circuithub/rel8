{-# language DataKinds #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Type.Array1D
  ( Array1D( Array1D )
  , NotArray
  , getArray1D
  )
where

-- aeson
import Data.Aeson
  ( ToJSON
  , ToJSON1
  , ToJSONKey
  , FromJSON
  , FromJSON1
  , FromJSONKey
  )

-- base
import Control.Applicative ( Alternative, (<|>) )
import Control.Monad ( MonadPlus )
import Data.Functor.Classes ( Eq1, Ord1, Read1, Show1 )
import Data.Kind ( Type )
import GHC.Exts ( IsList )
import Prelude hiding ( null )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Ord ( DBMax, DBMin, DBOrd )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )

-- semigroupoids
import Data.Functor.Alt ( Alt, (<!>) )
import Data.Functor.Apply ( Apply )
import Data.Functor.Bind ( Bind )
import Data.Functor.Plus ( Plus )

-- semialign
import Data.Align ( Align )
import Data.Semialign ( Semialign )
import Data.Zip ( Repeat, Unzip, Zip )


-- | A one dimensional array.
newtype Array1D a = Array1D [a]
  deriving stock Traversable
  deriving newtype
    ( Eq, Ord, Read, Show, Semigroup, Monoid, IsList
    , Functor, Foldable
    , Eq1, Ord1, Read1, Show1
    , FromJSON1, ToJSON1, FromJSON, FromJSONKey, ToJSON, ToJSONKey
    , Apply, Applicative, Alternative, Plus, Bind, Monad, MonadPlus
    , Align, Semialign, Repeat, Unzip, Zip
    )


instance Alt Array1D where
  (<!>) = (<|>)


getArray1D :: Array1D a -> [a]
getArray1D (Array1D a) = a


type IsArray1D :: Type -> Bool
type family IsArray1D a where
  IsArray1D (Array1D _) = 'True
  IsArray1D _ = 'False


class IsArray1D a ~ 'False => NotArray a
instance IsArray1D a ~ 'False => NotArray a


array1DTypeInformation :: Sql NotArray a
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation (Array1D a)
array1DTypeInformation nullity info =
  case info of
    TypeInformation{ encode, decode, typeName } -> TypeInformation
      { decode = case nullity of
          Null -> Array1D <$> Hasql.listArray (Hasql.nullable decode)
          NotNull -> Array1D <$> Hasql.listArray (Hasql.nonNullable decode)
      , encode = case nullity of
          Null -> Opaleye.ArrayExpr . fmap (maybe null encode) . getArray1D
          NotNull -> Opaleye.ArrayExpr . fmap encode . getArray1D
      , typeName = typeName <> "[]"
      }
  where
    null = Opaleye.ConstExpr Opaleye.NullLit


instance (Sql DBType a, Sql NotArray a) => DBType (Array1D a) where
  typeInformation = array1DTypeInformation nullable typeInformation


instance (Sql DBEq a, Sql NotArray a) => DBEq (Array1D a)


instance (Sql DBOrd a, Sql NotArray a) => DBOrd (Array1D a)


instance (Sql DBMax a, Sql NotArray a) => DBMax (Array1D a)


instance (Sql DBMin a, Sql NotArray a) => DBMin (Array1D a)


instance (Sql DBType a, Sql NotArray a) => DBSemigroup (Array1D a) where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance (Sql DBType a, Sql NotArray a) => DBMonoid (Array1D a) where
  memptyExpr = litExpr mempty
