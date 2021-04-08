{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  , dbTypeNullity
  , dbTypeDict
  , fromNullityDict
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ), Sql, nullable )
import Rel8.Schema.Spec ( Spec( Spec ) )


type ConstrainDBType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall c labels necessity a. ()
     => (spec ~ 'Spec labels necessity a)
     => (forall x. (constraint x => c x)) => Sql c a
  )
  => ConstrainDBType constraint spec
instance
  ( spec ~ 'Spec labels necessity a
  , Sql constraint a
  )
  => ConstrainDBType constraint spec


dbTypeNullity :: Dict (ConstrainDBType c) ('Spec l n a) -> Nullity a
dbTypeNullity = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Nullity a
    step2 Dict = nullable


dbTypeDict :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict c (Unnullify a)
dbTypeDict = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Dict c (Unnullify a)
    step2 Dict = Dict


fromNullityDict :: Nullity a -> Dict c (Unnullify a) -> Dict (ConstrainDBType c) ('Spec l n a)
fromNullityDict Null Dict = Dict
fromNullityDict NotNull Dict = Dict
