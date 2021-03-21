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
import Data.Kind ( Constraint )
import Prelude hiding ( null )

-- rel8
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Insertion( RequiredInsert, OptionalInsert )
  , Name( Name )
  , Result( Result )
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )


type Labelable :: Context -> Constraint
class Labelable context where
  labeler :: ()
    => context ('Spec labels necessity nullability blueprint)
    -> context ('Spec (label ': labels) necessity nullability blueprint)

  unlabeler :: ()
    => context ('Spec (label ': labels) necessity nullability blueprint)
    -> context ('Spec labels necessity nullability blueprint)


instance Labelable Aggregation where
  labeler (Aggregation aggregate) = Aggregation aggregate
  unlabeler (Aggregation aggregate) = Aggregation aggregate


instance Labelable DB where
  labeler (DB a) = DB a
  unlabeler (DB a) = DB a


instance Labelable Insertion where
  labeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma

  unlabeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma


instance Labelable Name where
  labeler (Name name) = Name name
  unlabeler (Name name) = Name name


instance Labelable Result where
  labeler (Result a) = Result a
  unlabeler (Result a) = Result a
