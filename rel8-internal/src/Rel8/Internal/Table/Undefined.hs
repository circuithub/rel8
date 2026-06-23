{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Table.Undefined
  ( undefined
  )
where

-- base
import Prelude hiding ( undefined )

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Null ( snull, unsafeUnnullify )
import Rel8.Internal.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Internal.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Internal.Schema.Spec ( Spec(..) )
import Rel8.Internal.Table ( Table, fromColumns )


undefined :: Table Expr a => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  Spec {nullity, info} -> case nullity of
    Null -> snull info
    NotNull -> unsafeUnnullify (snull info)
