{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Enum
  ( Enum(..)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:+:)( L1, R1 ), M1( M1 ), U1( U1 ), Meta( MetaCons )
  , D, C
  )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude hiding ( Enum )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )

-- text
import Data.Text ( unpack )


-- | A deriving-via helper type for column types that store an \"enum\" type
-- (in Haskell terms, a sum type where all constructors are nullary) using a
-- Postgres @enum@ type.
--
-- Note that this must map to a specific type in your database's schema
-- (explicitly created with @CREATE TYPE ... AS ENUM@). The @name@ parameter
-- of @Enum@ should be the name of this type. Note also the names of the
-- Postgres @enum@'s values are assumed to match exactly the names of the
-- constructors of the Haskell type (up to and including case sensitivity).
type Enum :: Symbol -> Type -> Type
newtype Enum name a = Enum
  { unEnum :: a
  }


instance (KnownSymbol name, Generic a, GEnum (Rep a)) => DBType (Enum name a)
 where
  typeInformation = TypeInformation
    { decode = Hasql.enum ((`lookup` (fmap (Enum . to) <$> gdecode)) . unpack)
    , encode = Opaleye.ConstExpr . Opaleye.StringLit . gencode . from . unEnum
    , typeName = symbolVal (Proxy @name)
    }


type GEnum :: (Type -> Type) -> Constraint
class GEnum rep where
  gdecode :: [(String, rep x)]
  gencode :: rep x -> String


instance GEnum rep => GEnum (M1 D c rep) where
  gdecode = fmap M1 <$> gdecode
  gencode (M1 a) = gencode a


instance (GEnum a, GEnum b) => GEnum (a :+: b) where
  gdecode = (fmap L1 <$> gdecode) <> (fmap R1 <$> gdecode)
  gencode (L1 a) = gencode a
  gencode (R1 a) = gencode a


instance (KnownSymbol name, a ~ U1) =>
  GEnum (M1 C ('MetaCons name f r) a)
 where
  gdecode = [(symbolVal (Proxy @name), M1 U1)]
  gencode _ = symbolVal (Proxy @name)
