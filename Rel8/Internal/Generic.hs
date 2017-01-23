{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Internal.Generic where

import Control.Applicative (liftA2)
import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import GHC.Generics (K1(..), M1(..), (:*:)(..))
import GHC.TypeLits (symbolVal, KnownSymbol)
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Table as O
import Prelude hiding (not, id)
import Rel8.Internal.Expr
import Rel8.Internal.Types

--------------------------------------------------------------------------------
-- | The class of values that can be traversed for 'O.PrimExpr's.

class MapPrimExpr s where
  mapPrimExpr :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> s -> f s

instance MapPrimExpr (Expr column) where
  mapPrimExpr f (Expr a) = fmap Expr (f a)


--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (SchemaInfo name def t) where
  schema = SchemaInfo (symbolVal (Proxy @name))


--------------------------------------------------------------------------------
class GTraverseSchema schema expr | schema -> expr where
  gtraverseSchema
    :: Applicative f
    => (forall a. String -> f (Expr a)) -> schema x -> f (expr y)

instance (GTraverseSchema schema expr) =>
         GTraverseSchema (M1 i c schema) (M1 i c expr) where
  gtraverseSchema f (M1 a) = M1 <$> gtraverseSchema f a

instance (GTraverseSchema fSchema fExpr, GTraverseSchema gSchema gExpr) =>
         GTraverseSchema (fSchema :*: gSchema) (fExpr :*: gExpr) where
  gtraverseSchema f (l :*: r) =
    liftA2 (:*:) (gtraverseSchema f l) (gtraverseSchema f r)

instance GTraverseSchema (K1 i (SchemaInfo name def a)) (K1 i (Expr a)) where
  gtraverseSchema f (K1 (SchemaInfo a)) = K1 <$> f a


--------------------------------------------------------------------------------
-- | Form 'O.Writer's from a schema specification
class Writer (f :: * -> *) schema expr | f schema -> expr where
  columnWriter :: proxy f -> schema a -> O.Writer (expr a) ()

instance (Writer f schema expr) =>
         Writer f (M1 i c schema) (M1 i c expr) where
  columnWriter p (M1 s) = lmap (\(M1 a) -> a) (columnWriter p s)

instance (Writer f fSchema fExpr, Writer f gSchema gExpr) =>
         Writer f (fSchema :*: gSchema) (fExpr :*: gExpr) where
  columnWriter p (l :*: r) =
    dimap (\(l' :*: r') -> (l', r')) fst (columnWriter p l ***! columnWriter p r)

instance Writer Expr (K1 i (SchemaInfo name d a)) (K1 i (Expr a)) where
  columnWriter _ (K1 (SchemaInfo name)) =
    dimap
      (\(K1 expr) -> exprToColumn expr)
      (const ())
      (O.required name)

instance Writer Insert (K1 i (SchemaInfo name 'NoDefault a)) (K1 i (Expr a)) where
  columnWriter _ (K1 (SchemaInfo name)) =
    dimap
      (\(K1 expr) -> exprToColumn expr)
      (const ())
      (O.required name)

instance Writer Insert (K1 i (SchemaInfo name 'HasDefault a)) (K1 i (Default (Expr a))) where
  columnWriter _ (K1 (SchemaInfo name)) =
    dimap
      (\(K1 def) ->
         case def of
           InsertDefault -> O.Column O.DefaultInsertExpr
           OverrideDefault expr -> exprToColumn expr)
      (const ())
      (O.required name)
