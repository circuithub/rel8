{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Table.Undefined
  ( undefined
  )
where

-- base
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Null ( snull, unsafeUnnullify )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table ( Table, fromColumns )


undefined :: Table Expr a => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  SSpec {nullability, info} -> case nullability of
    Nullable -> DB (snull Nullable info)
    NonNullable -> DB (unsafeUnnullify (snull Nullable info))
