{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Rel8.Anonymous where

import Data.Profunctor
import Data.Proxy
import Data.Tagged
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (field)
import GHC.TypeLits
import Labels
import Labels.Internal ((:=))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Table as O hiding (required)
import Rel8

newtype Row a = Row { getRow :: a }
  deriving (Has k v)

instance (KnownSymbol c1, FromField v1) => Table (Row (c1 := Expr v1)) (Row (c1 := v1)) where
  rowParser = Row <$> fmap (Proxy :=) field
  columnCount = Tagged 1
  traversePrimExprs f (Row (p := (Expr a))) = Row . (p :=) . Expr <$> f a

instance (KnownSymbol c1,KnownSymbol c2,FromField v1,FromField v2) => Table (Row (c1 := Expr v1,c2 := Expr v2)) (Row (c1 := v1,c2 := v2)) where
  rowParser = Row <$> ((,) <$> fmap (Proxy :=) field <*> fmap (Proxy :=) field)
  columnCount = Tagged 2
  traversePrimExprs f (Row (p := (Expr a),p2 := (Expr b))) =
    Row <$> ((,) <$> ((p :=) . Expr <$> f a) <*> ((p2 :=) . Expr <$> f b))

class AnonymousTable a where
  makeColumns :: proxy a -> a

instance (KnownSymbol c1, FromField v1, e1 ~ Expr v1) => AnonymousTable (c1 := e1) where
  makeColumns _ = (Proxy := Expr (O.BaseTableAttrExpr (symbolVal (Proxy @c1))))

instance (KnownSymbol c1, FromField v1, KnownSymbol c2, FromField v2, e1 ~ Expr v1, e2 ~ Expr v2) => AnonymousTable (c1 := e1, c2 := e2) where
  makeColumns _ = (Proxy := Expr (O.BaseTableAttrExpr (symbolVal (Proxy @c1))), Proxy := Expr (O.BaseTableAttrExpr (symbolVal (Proxy @c1))))

selectFrom :: forall columns haskell.
              (AnonymousTable columns,Table (Row columns) (Row haskell))
           => String -> Query (Row columns)
selectFrom n =
  O.queryTableExplicit
    (O.ColumnMaker (lmap Row (O.PackMap traversePrimExprs)))
    (O.Table n
             (O.TableProperties (O.Writer (pure ()))
                                (O.View (makeColumns (Proxy @columns)))))
