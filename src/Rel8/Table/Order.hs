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
import Rel8.Expr ( unDB )
import Rel8.Expr.Order ( asc, desc, nullsFirst, nullsLast )
import Rel8.Order ( Order )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable (htabulateA, hfield, hspecs)
import Rel8.Schema.Nullability
  ( Unnullify, Nullability( Nullable, NonNullable )
  , Sql, nullabilization
  )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table
import Rel8.Table.Ord
import Rel8.Type.Ord


-- | Construct an 'Order' for a 'Table' by sorting all columns into ascending
-- orders (any nullable columns will be sorted with @NULLS FIRST@).
ascTable :: forall a. OrdTable a => Order a
ascTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (ordTable @a) field of
      dict@Dict -> case ord dict of
        Dict -> Const $ unDB . (`hfield` field) >$< case nullability dict of
          Nullable -> nullsFirst asc
          NonNullable -> asc


-- | Construct an 'Order' for a 'Table' by sorting all columns into descending
-- orders (any nullable columns will be sorted with @NULLS LAST@).
descTable :: forall a. OrdTable a => Order a
descTable = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (ordTable @a) field of
      dict@Dict -> case ord dict of
        Dict -> Const $ unDB . (`hfield` field) >$< case nullability dict of
          Nullable -> nullsLast desc
          NonNullable -> desc


nullability :: Dict (ConstrainDBType DBOrd) ('Spec l n a) -> Nullability a
nullability = step2 . step1
  where
    step1 :: Dict (ConstrainDBType DBOrd) ('Spec l n a) -> Dict (Sql DBOrd) a
    step1 Dict = Dict

    step2 :: Dict (Sql DBOrd) a -> Nullability a
    step2 Dict = nullabilization


ord :: Dict (ConstrainDBType DBOrd) ('Spec l n a) -> Dict DBOrd (Unnullify a)
ord = step2 . step1
  where
    step1 :: Dict (ConstrainDBType DBOrd) ('Spec l n a) -> Dict (Sql DBOrd) a
    step1 Dict = Dict

    step2 :: Dict (Sql DBOrd) a -> Dict DBOrd (Unnullify a)
    step2 Dict = Dict
