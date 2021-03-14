{-# language TypeFamilies #-}

module Rel8.Table.Undefined
  ( undefined
  )
where

-- base
import Prelude hiding ( undefined )

-- rel8
import Rel8.Expr ( snull )
import Rel8.Expr.Null ( unsafeSemiunnullify )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Table ( Table, Context, fromColumns )


undefined :: (Table a, Context a ~ DB) => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  SSpec _ _ _ info -> DB (unsafeSemiunnullify (snull info))
