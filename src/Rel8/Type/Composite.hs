{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Composite
  ( Composite( Composite )
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:*:)( (:*:) ), K1( K1 ), M1( M1 )
  )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude hiding ( null )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null ( Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


-- | A deriving-via helper type for column types that store a Haskell product
-- type in a single Postgres column using a Postgres composite type.
--
-- Note that this must map to a specific extant type in your database's schema
-- (created with @CREATE TYPE@). The @name@ parameter of @Composite@ should be
-- the name of this type.
type Composite :: Symbol -> Type -> Type
newtype Composite name a = Composite
  { unComposite :: a
  }


instance (KnownSymbol name, Generic a, GComposite (Rep a)) =>
  DBType (Composite name a)
 where
  typeInformation = TypeInformation
    { decode = Hasql.composite (Composite . to <$> gdecode @(Rep a))
    , encode = Opaleye.FunExpr "ROW" . gencode @(Rep a) . from . unComposite
    , typeName = symbolVal (Proxy @name)
    }


type GComposite :: (Type -> Type) -> Constraint
class GComposite rep where
  gdecode :: Hasql.Composite (rep x)
  gencode :: rep x -> [Opaleye.PrimExpr]


instance GComposite rep => GComposite (M1 i c rep) where
  gdecode = M1 <$> gdecode
  gencode (M1 a) = gencode a


instance (GComposite a, GComposite b) => GComposite (a :*: b) where
  gdecode = liftA2 (:*:) gdecode gdecode
  gencode (a :*: b) = gencode a <> gencode b


instance Sql DBType a => GComposite (K1 i a) where
  gdecode = fmap K1 $ Hasql.field $ case nullable @a of
    Null -> Hasql.nullable (decode typeInformation)
    NotNull -> Hasql.nonNullable (decode typeInformation)
  gencode (K1 a) = case nullable @a of
    Null -> [maybe null (encode typeInformation) a]
    NotNull -> [encode typeInformation a]
    where
      null = Opaleye.ConstExpr Opaleye.NullLit
