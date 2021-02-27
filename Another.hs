{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

{-# options -Wno-implicit-prelude -Wno-missing-export-lists -Wno-missing-import-lists #-}

module Another where

import Prelude hiding (null)
import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Database.PostgreSQL.Simple.FromField (FieldParser)
import Database.PostgreSQL.Simple.FromRow (RowParser, fieldWith, field)
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.RunQuery as O
import qualified Opaleye as O
import Data.Profunctor.Product.Default (Default)
import Data.Proxy ( Proxy( Proxy ) )

type family Column (f :: Type -> Type) (a :: Type) :: Type where
  Column Identity a = a
  Column f a = f a

newtype C f a = C { toColumn :: Column f a }

data Witness (c :: Type -> Constraint) (a :: Type) where
  Witness :: c a => Witness c a

class DBType (a :: Type) where
  typeInformation :: TypeInformation a

data TypeInformation a = TypeInformation
  { encode :: a -> O.PrimExpr
  , decode :: FieldParser a
  }

mapTypeInformation :: (a -> b) -> (b -> a) -> TypeInformation a -> TypeInformation b
mapTypeInformation f g TypeInformation{ encode, decode } = TypeInformation
  { encode = encode . g
  , decode = \x y -> f <$> decode x y
  }

newtype OpaleyeDBType sql a = OpaleyeDBType a

instance (O.DefaultFromField sql a, Default O.ToFields a (O.Column sql)) => DBType (OpaleyeDBType sql a) where
  typeInformation = TypeInformation
    { encode = \(OpaleyeDBType x) -> 
        case O.toFields @a @(O.Column sql) x of
          O.Column primExpr -> primExpr
    , decode = \x y -> 
        case O.defaultFromField @sql @a of
          O.QueryRunnerColumn _ fieldParser -> OpaleyeDBType <$> fieldParser x y
    }

deriving via OpaleyeDBType O.SqlBool Bool instance DBType Bool
deriving via OpaleyeDBType O.SqlInt4 Int instance DBType Int

instance DBType ()  where
  typeInformation = TypeInformation
    { encode = \_ -> O.ConstExpr O.NullLit
    , decode = \_ _ -> return ()
    }

instance DBType a => DBType (Maybe a) where
  typeInformation = TypeInformation
    { encode = maybe null (encode typeInformation)
    , decode = \x y ->
        case y of
          Nothing -> return Nothing
          Just _  -> decode typeInformation x y
    }

class AllFields t DBType => HigherKindedTable (t :: (Type -> Type) -> Type) where
  type HField t = (i :: Type -> Type) | i -> t
  type AllFields t (c :: Type -> Constraint) :: Constraint

  hindex :: t f -> HField t a -> C f a
  htabulate :: (forall x. HField t x -> C f x) -> t f
  htraverse :: Applicative m => (forall x. C f x -> m (C g x)) -> t f -> m (t g)
  hdicts :: forall (c :: Type -> Constraint). AllFields t c => t (Witness c)

class HigherKindedTable (Schema t) => Table (t :: Type) where
  type Schema t :: ((Type -> Type) -> Type)
  type Context t :: Type -> Type

  fromColumns :: Schema t (Context t) -> t
  toColumns :: t -> Schema t (Context t)

instance HigherKindedTable f => Table (f g) where
  type Schema (f g) = f
  type Context (f g) = g
  toColumns = id
  fromColumns = id

newtype Expr a = Expr { toPrimExpr :: O.PrimExpr }

unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a

data MaybeTable t = MaybeTable (Expr (Maybe ())) t

class SerializationMethod expr haskell => Serializable expr haskell | expr -> haskell, haskell -> expr where

instance SerializationMethod expr haskell => Serializable expr haskell 

type family ResultType (expr :: Type) :: Type where
  ResultType (MaybeTable t) = Maybe (ResultType t)
  ResultType (t Expr)       = t Identity
  ResultType (a, b)         = (ResultType a, ResultType b)
  ResultType (Expr a)       = a

type family ExprType (haskell :: Type) :: Type where
  ExprType (Maybe (t Identity))    = MaybeTable (t Expr)
  ExprType (Maybe a)               = Expr (Maybe a)
  ExprType (t Identity)            = t Expr
  ExprType (a, b)                  = (ExprType a, ExprType b)
  ExprType a                       = Expr a

class (haskell ~ ResultType expr, expr ~ ExprType haskell) => SerializationMethod (expr :: Type) (haskell :: Type) where
  rowParser :: RowParser haskell
  lit :: haskell -> expr

instance (DBType a, a ~ b, ExprType b ~ Expr a) => SerializationMethod (Expr (a :: Type)) (b :: Type) where
  rowParser = fieldWith (decode (typeInformation @a))
  lit = Expr . encode (typeInformation @a)

instance (HigherKindedTable s, s ~ t) => SerializationMethod (s Expr) (t Identity) where
  rowParser = htraverse f $ htabulate @s \i -> g (hindex hdicts i) 
    where
      f :: C RowParser x -> RowParser (C Identity x)
      f (C x) = C <$> x

      g :: forall a. C (Witness DBType) a -> C RowParser a
      g (C Witness) = C $ fieldWith $ decode $ typeInformation @a

  lit t = htabulate \i -> f (hindex hdicts i) (hindex t i) 
    where
      f :: forall x. C (Witness DBType) x -> C Identity x -> C Expr x
      f (C Witness) (C a) = C $ Expr $ encode (typeInformation @x) a

instance (SerializationMethod a1 b1, SerializationMethod a2 b2, a1 ~ ExprType b1, a2 ~ ExprType b2, b1 ~ ResultType a1, b2 ~ ResultType a2) => SerializationMethod (a1, a2) (b1, b2) where
  rowParser = liftA2 (,) rowParser rowParser
  lit (a, b) = (lit a, lit b)

instance (Serializable s t, Table s, Context s ~ Expr, MaybeTable s ~ ExprType (Maybe t), ResultType s ~ t) => SerializationMethod (MaybeTable s) (Maybe t) where
  rowParser = do
    nullTag <- field
    case nullTag of
      Just ()   -> Just <$> rowParser
      Nothing   -> Nothing <$ htraverse @(Schema s) (\x -> x <$ fieldWith (\_ _ -> pure ())) (htabulate @_ @Proxy \_ -> C Proxy)

  lit Nothing = MaybeTable nullUnit $ fromColumns $ htabulate \_ -> C $ unsafeCoerceExpr nullUnit
    where
      nullUnit :: Expr (Maybe ())
      nullUnit = lit Nothing

  lit (Just a) = MaybeTable (lit (Just ())) (lit a)

null :: O.PrimExpr
null = O.ConstExpr O.NullLit

--

data One f = One { oneA :: Column f Int }

data OneField :: Type -> Type where
  OneA :: OneField Int

instance HigherKindedTable One where
  type HField One = OneField
  type AllFields One c = (c Int)

  hindex One{ oneA } = \case
    OneA -> C oneA

  htabulate f = One { oneA = toColumn $ f OneA }

  hdicts = One Witness

  htraverse :: forall m f g. Applicative m => (forall x. C f x -> m (C g x)) -> One f -> m (One g)
  htraverse f One{ oneA } = fmap (\(C oneA') -> One oneA') $ f (C oneA :: C f Int)


-- TODO Get this to infer!
-- thing :: _
-- thing = lit (Just $ (One 42))

thing :: MaybeTable (One Expr)
thing = lit (Just $ (One 42 :: One Identity))

-- thing2 :: MaybeTable (One Expr)
-- thing2 = lit (Just _)
