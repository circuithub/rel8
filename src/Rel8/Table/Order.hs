{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Table.Order
  ( ascTable
  , descTable
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( (>$<), contramap )
import Prelude

-- rel8
import Rel8.Expr.Order ( asc, desc, nullsFirst, nullsLast )
import Rel8.Order ( Order )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable (htabulateA, hfield, hdicts, hspecs)
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table
import Rel8.Table.Ord
import Rel8.Type.Ord
import Rel8.Expr (unDB)


ascTable :: forall a. OrdTable a => Order a
ascTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {nullability} -> case hfield ords field of
      Dict -> Const $ unDB . (`hfield` field) >$< case nullability of
        Nullable -> nullsFirst asc
        NonNullable -> asc
    where
      ords = hdicts @(Columns a) @(ConstrainDBType DBOrd)


descTable :: forall a. OrdTable a => Order a
descTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {nullability} -> case hfield ords field of
      Dict -> Const $ unDB . (`hfield` field) >$< case nullability of
        Nullable -> nullsLast desc
        NonNullable -> desc
    where
      ords = hdicts @(Columns a) @(ConstrainDBType DBOrd)
