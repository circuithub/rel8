{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Internal.Table.Order
  ( ascTable
  , descTable
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( (>$<), contramap )
import Prelude

-- rel8
import Rel8.Internal.Expr.Order ( asc, desc, nullsFirst, nullsLast )
import Rel8.Internal.Order ( Order )
import Rel8.Internal.Schema.Dict ( Dict( Dict ) )
import Rel8.Internal.Schema.HTable (htabulateA, hfield, hspecs)
import Rel8.Internal.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Internal.Schema.Spec ( Spec( Spec, nullity ) )
import Rel8.Internal.Table ( Columns, toColumns )
import Rel8.Internal.Table.Ord ( OrdTable, ordTable )


-- | Construct an 'Order' for a 'Table' by sorting all columns into ascending
-- orders (any nullable columns will be sorted with @NULLS FIRST@).
ascTable :: forall a. OrdTable a => Order a
ascTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    Spec {nullity} -> case hfield (ordTable @a) field of
      Dict -> Const $ (`hfield` field) >$<
        case nullity of
          Null -> nullsFirst asc
          NotNull -> asc


-- | Construct an 'Order' for a 'Table' by sorting all columns into descending
-- orders (any nullable columns will be sorted with @NULLS LAST@).
descTable :: forall a. OrdTable a => Order a
descTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    Spec {nullity} -> case hfield (ordTable @a) field of
      Dict -> Const $ (`hfield` field) >$<
        case nullity of
          Null -> nullsLast desc
          NotNull -> desc
