{-# language DataKinds #-}
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
import Rel8.Expr ( unE )
import Rel8.Expr.Order ( asc, desc, nullsFirst, nullsLast )
import Rel8.Order ( Order )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable (htabulateA, hfield, hspecs)
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec( SSpec, nullity ) )
import Rel8.Table ( Columns, toColumns )
import Rel8.Table.Ord ( OrdTable, ordTable )


-- | Construct an 'Order' for a 'Table' by sorting all columns into ascending
-- orders (any nullable columns will be sorted with @NULLS FIRST@).
ascTable :: forall a. OrdTable a => Order a
ascTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {nullity} -> case hfield (ordTable @a) field of
      Dict -> Const $ unE . (`hfield` field) >$<
        case nullity of
          Null -> nullsFirst asc
          NotNull -> asc


-- | Construct an 'Order' for a 'Table' by sorting all columns into descending
-- orders (any nullable columns will be sorted with @NULLS LAST@).
descTable :: forall a. OrdTable a => Order a
descTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {nullity} -> case hfield (ordTable @a) field of
      Dict -> Const $ unE . (`hfield` field) >$<
        case nullity of
          Null -> nullsLast desc
          NotNull -> desc
