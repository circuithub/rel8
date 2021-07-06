{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.Rel8able
  (
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Expr ( Expr )
import qualified Rel8.Kind.Algebra as K
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GColumns, gfromColumns, gtoColumns
  , GFromExprs, gfromResult, gtoResult
  )
import Rel8.Schema.Context.Virtual ( Virtual, virtual )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HConstrainTable, hdicts )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.ADT ( ADT )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Serialize ( ToExprs )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Ord ( DBOrd )


instance (Rel8able t, Virtual context, context ~ context') =>
  Table context' (t context)
 where
  type Columns (t context) = GColumns t
  type Context (t context) = context
  type FromExprs (t context) = GFromExprs t
  type Transpose to (t context) = t to

  fromColumns = gfromColumns virtual
  toColumns = gtoColumns virtual
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
