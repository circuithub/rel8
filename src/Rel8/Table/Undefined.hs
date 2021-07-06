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
import Rel8.Expr ( Expr )
import Rel8.Expr.Null ( snull, unsafeUnnullify )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Table ( Table, fromColumns )


undefined :: Table Expr a => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  Spec {nullity, info} -> case nullity of
    Null -> snull info
    NotNull -> unsafeUnnullify (snull info)
