{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Internal.Table.Rel8able
  (
  )
where

-- base
import Prelude ()

-- base-compat
import Data.Type.Equality.Compat

-- rel8
import Rel8.Internal.Expr ( Expr )
import qualified Rel8.Internal.Kind.Algebra as K
import Rel8.Internal.Kind.Context ( Reifiable, contextSing )
import Rel8.Internal.Generic.Rel8able
  ( Rel8able, Algebra
  , GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.HTable ( HConstrainTable, hdicts )
import Rel8.Internal.Schema.Null ( Sql )
import Rel8.Internal.Schema.Result ( Result )
import Rel8.Internal.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Internal.Table.ADT ( ADT )
import Rel8.Internal.Table.Eq ( EqTable, eqTable )
import Rel8.Internal.Table.Ord ( OrdTable, ordTable )
import Rel8.Internal.Table.Serialize ( ToExprs )
import Rel8.Internal.Type.Eq ( DBEq )
import Rel8.Internal.Type.Ord ( DBOrd )


instance (Rel8able t, Reifiable context, context ~ context') =>
  Table context' (t context)
 where
  type Columns (t context) = GColumns t
  type Context (t context) = context
  type FromExprs (t context) = GFromExprs t
  type Transpose to (t context) = t to

  fromColumns = gfromColumns contextSing
  toColumns = gtoColumns contextSing
  fromResult = gfromResult @t
  toResult = gtoResult @t


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (Sql DBEq)
  )
  => EqTable (t context)
 where
  eqTable = hdicts @(Columns (t context)) @(Sql DBEq)


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (Sql DBEq)
  , HConstrainTable (Columns (t context)) (Sql DBOrd)
  )
  => OrdTable (t context)
 where
  ordTable = hdicts @(Columns (t context)) @(Sql DBOrd)


instance
  ( Rel8able t', t' ~ Choose (Algebra t) t
  , x ~ t' Expr
  , result ~ Result
  )
  => ToExprs x (t result)


type Choose :: K.Algebra -> K.Rel8able -> K.Rel8able
type family Choose algebra t where
  Choose 'K.Sum t = ADT t
  Choose 'K.Product t = t
