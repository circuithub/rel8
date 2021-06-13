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
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Null ( snull, unsafeUnnullify )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table ( Table, fromColumns )


undefined :: Table Expr a => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  SSpec {nullity, info} -> case nullity of
    Null -> E (snull info)
    NotNull -> E (unsafeUnnullify (snull info))
