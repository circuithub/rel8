{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.FieldName ( FieldName(..), Col( FN ), fieldNames, project ) where

import Prelude
import Rel8.Schema.Context
import Rel8.Schema.HTable
import Rel8.Schema.Spec
import Rel8.Table
import Data.Kind ( Type )
import Rel8.Expr ( Expr )
import Rel8.Table.Recontextualize
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.HTable.MapTable
import Rel8.Schema.HTable.Vectorize
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify
import Rel8.Type ( DBType )

type FieldName :: Type -> k -> Type
data FieldName a x where
  FieldName :: forall (a :: Type) (x :: Type). HField (Columns a) ('Spec x) -> FieldName a x

instance Sql DBType a => Table (FieldName t) (FieldName t a) where
  type Columns (FieldName t a) = HType a
  type Context (FieldName t a) = FieldName t
  toColumns (FieldName i) = HType (FN i)
  fromColumns (HType (FN i)) = FieldName i
  unreify = notReify
  reify = notReify

instance Interpretation (FieldName a) where
  data Col (FieldName a) _spec where
    FN :: HField (Columns a) spec -> Col (FieldName a) spec

instance Sql DBType a => Recontextualize Expr (FieldName t) (Expr a) (FieldName t a) where

instance Sql DBType a => Recontextualize Name (FieldName t) (Name a) (FieldName t a) where

instance Sql DBType a => Recontextualize (FieldName t) Expr (FieldName t a) (Expr a) where

instance Sql DBType a => Recontextualize (FieldName t) (FieldName t) (FieldName t a) (FieldName t a) where

instance Sql DBType a => Recontextualize (FieldName t) Name (FieldName t a) (Name a) where

fieldNames :: (Congruent a b, Table (FieldName a) b) => b
fieldNames = fromColumns (htabulate FN)

-- | Project a subset of columns from a 'Table'.
project :: (Recontextualize f (FieldName a) a a', Recontextualize (FieldName a) f b' b) => (a' -> b') -> a -> b
project f x = fromColumns $ htabulate \i ->
  case hfield selected i of
    FN j -> hfield (toColumns x) j
  where
    selected = toColumns $ f fieldNames
