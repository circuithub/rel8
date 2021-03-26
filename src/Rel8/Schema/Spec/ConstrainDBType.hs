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
  , dbTypeNullability
  , dbTypeDict
  , fromNullabilityDict
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Nullability
  ( Unnullify
  , Nullability
  , Sql, nullabilization, toSql
  )
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


dbTypeNullability :: Dict (ConstrainDBType c) ('Spec l n a) -> Nullability a
dbTypeNullability = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Nullability a
    step2 Dict = nullabilization


dbTypeDict :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict c (Unnullify a)
dbTypeDict = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Dict c (Unnullify a)
    step2 Dict = Dict


fromNullabilityDict :: Nullability a -> Dict c (Unnullify a) -> Dict (ConstrainDBType c) ('Spec l n a)
fromNullabilityDict nullability = step2 . toSql nullability
  where
    step2 :: Dict (Sql c) a -> Dict (ConstrainDBType c) ('Spec l n a)
    step2 Dict = Dict
