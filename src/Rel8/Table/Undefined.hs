{-# language TypeFamilies #-}

module Rel8.Table.Undefined
  ( undefined
  )
where

-- base
import Prelude hiding ( null, undefined )

-- rel8
import Rel8.Expr.Null ( null, unsafeSemiunnullify )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.HTable ( htabulate, hfield, hspecs )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Table ( Table, Context, fromColumns )
import Rel8.Type ( withDBType )


undefined :: (Table a, Context a ~ DB) => a
undefined = fromColumns $ htabulate $ \field -> case hfield hspecs field of
  SSpec _ _ _ info -> withDBType info $ DB (unsafeSemiunnullify null)
