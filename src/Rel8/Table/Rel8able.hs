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
  , Lower
  )
import Rel8.Schema.Context.Virtual ( Virtual, virtual )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HConstrainTable, hdicts )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, Congruent, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Schema.Spec.Constrain ( ConstrainSpec )
import Rel8.Table.ADT ( ADT, Raise )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( ToExprs )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Ord ( DBOrd )


instance (Rel8able t, Virtual f, f ~ Raise g, g ~ Lower f) => Table f (t g) where
  type Columns (t g) = GColumns t
  type Context (t g) = Raise g
  type FromExprs (t g) = GFromExprs t

  fromColumns = gfromColumns virtual
  toColumns = gtoColumns virtual
  fromResult = gfromResult @t
  toResult = gtoResult @t


instance
  ( Rel8able t
  , from' ~ Raise from
  , to' ~ Raise to
  , Virtual from'
  , Virtual to'
  , Congruent (t from) (t to)
  , Lower from' ~ from
  , Lower to' ~ to
  )
  => Recontextualize from' to' (t from) (t to)


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (ConstrainSpec (Sql DBEq))
  )
  => EqTable (t context)
 where
  eqTable = hdicts @(Columns (t context)) @(ConstrainSpec (Sql DBEq))


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (ConstrainSpec (Sql DBEq))
  , HConstrainTable (Columns (t context)) (ConstrainSpec (Sql DBOrd))
  )
  => OrdTable (t context)
 where
  ordTable = hdicts @(Columns (t context)) @(ConstrainSpec (Sql DBOrd))


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
