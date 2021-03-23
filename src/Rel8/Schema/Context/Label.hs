{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema.Context.Label
  ( Labelable( labeler, unlabeler )
  )
where

-- base

-- base
import Data.Kind ( Constraint, Type )
import Prelude hiding ( null )

-- rel8
import Rel8.Aggregate ( Aggregate, Col( Aggregation ) )
import Rel8.Schema.Context
  ( Insertion( RequiredInsert, OptionalInsert )
  , Name
  , Col( Name )
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ), Col( Result ) )
import Rel8.Expr ( Expr, Col( DB ) )
import Data.Functor.Identity ( Identity )


type Labelable :: (Type -> Type) -> Constraint
class Labelable context where
  labeler :: ()
    => Col context ('Spec labels necessity db a)
    -> Col context ('Spec (label ': labels) necessity db a)

  unlabeler :: ()
    => Col context ('Spec (label ': labels) necessity db a)
    -> Col context ('Spec labels necessity db a)


instance Labelable Aggregate where
  labeler (Aggregation aggregate) = Aggregation aggregate
  unlabeler (Aggregation aggregate) = Aggregation aggregate


instance Labelable Expr where
  labeler (DB a) = DB a
  unlabeler (DB a) = DB a


instance Labelable Name where
  labeler (Name name) = Name name
  unlabeler (Name name) = Name name


instance Labelable Identity where
  labeler (Result a) = Result a
  unlabeler (Result a) = Result a
