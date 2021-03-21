{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Insert
  ( toInsert
  , toInsertDefaults
  )
where

-- base
import Prelude

-- rel8
import Rel8.Kind.Necessity ( SNecessity( SOptional, SRequired ) )
import Rel8.Schema.Context ( DB(..), Insertion(..) )
import Rel8.Schema.HTable ( hfield, htabulate, hspecs )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Inserts )


toInsert :: Inserts exprs inserts => exprs -> inserts
toInsert (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec _ necessity _ _ -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert (Just expr)


toInsertDefaults :: Inserts exprs inserts => exprs -> inserts
toInsertDefaults (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec _ necessity _ _ -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert Nothing
