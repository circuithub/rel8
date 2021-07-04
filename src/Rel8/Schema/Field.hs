{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema.Field
  ( Field(..)
  , fields
  )
where

-- base
import Data.Kind ( Type )
import Data.Type.Equality ( type (~~) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Schema.HTable ( HField, htabulate, hfield, hspecs )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec ) )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result( R ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | A special context used in the construction of 'Rel8.Projection's.
type Field :: Type -> k -> Type
data Field table a where
  Field :: a ~~ a' => HField (Columns table) ('Spec a') -> Field table a
  F :: !(Field table a) -> Field table ('Spec a)


instance Sql DBType a => Table (Field table) (Field table a) where
  type Columns (Field table a) = HType a
  type Context (Field table a) = Field table
  type FromExprs (Field table a) = a

  toColumns a = HType (F a)
  fromColumns (HType (F a)) = a
  toResult a = HType (R a)
  fromResult (HType (R a)) = a


instance Sql DBType a =>
  Recontextualize Aggregate (Field t) (Aggregate a) (Field t a)


instance Sql DBType a =>
  Recontextualize Expr (Field t) (Expr a) (Field t a)


instance Sql DBType a =>
  Recontextualize (Field t) Aggregate (Field t a) (Aggregate a)


instance Sql DBType a =>
  Recontextualize (Field t) Expr (Field t a) (Expr a)


instance Sql DBType a =>
  Recontextualize (Field t) (Field t) (Field t a) (Field t a)


instance Sql DBType a =>
  Recontextualize (Field t) Name (Field t a) (Name a)


instance Sql DBType a =>
  Recontextualize Name (Field t) (Name a) (Field t a)


fields :: Recontextualize context (Field table) table fields => fields
fields = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  SSpec {} -> F $ Field field
