{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Table.Order
  ( asc
  , desc
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Contravariant ( (>$<), contramap )
import Prelude

-- rel8
import qualified Rel8.Expr.Order as Expr
import Rel8.Kind.Nullability ( SNullability( SNullable, SNonNullable ) )
import Rel8.Order ( Order )
import Rel8.Schema.Context ( unDB )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table
import Rel8.Table.Ord
import Rel8.Type.Ord


asc :: forall a. OrdTable a => Order a
asc = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec _ _ nullability _ -> case hfield ords field of
      Dict -> Const $ unDB . (`hfield` field) >$< case nullability of
        SNullable -> Expr.nullsFirst Expr.asc
        SNonNullable -> Expr.asc
    where
      ords = hdicts @(Columns a) @(ConstrainDBType DBOrd)


desc :: forall a. OrdTable a => Order a
desc = contramap toColumns $ getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec _ _ nullability _ -> case hfield ords field of
      Dict -> Const $ unDB . (`hfield` field) >$< case nullability of
        SNullable -> Expr.nullsLast Expr.desc
        SNonNullable -> Expr.desc
    where
      ords = hdicts @(Columns a) @(ConstrainDBType DBOrd)
