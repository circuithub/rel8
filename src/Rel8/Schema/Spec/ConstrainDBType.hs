{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  , dbTypeNullity, dbTypeDict
  , nullifier, unnullifier
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Null
  ( Nullify, Unnullify
  , Nullity( Null, NotNull )
  , Nullable, Sql, nullable
  )
import Rel8.Schema.Serialize ( Exprable )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec( SSpec, exprable, nullity ) )


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
dbTypeNullity = step3 . step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Dict Nullable a
    step2 Dict = Dict

    step3 :: Dict Nullable a -> Nullity a
    step3 Dict = nullable


dbTypeDict :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict c (Unnullify a)
dbTypeDict = step2 . step1
  where
    step1 :: Dict (ConstrainDBType c) ('Spec l n a) -> Dict (Sql c) a
    step1 Dict = Dict

    step2 :: Dict (Sql c) a -> Dict c (Unnullify a)
    step2 Dict = Dict


fromNullityDict :: Nullity a -> Dict Exprable (Unnullify a) -> Dict c (Unnullify a) -> Dict (ConstrainDBType c) ('Spec l n a)
fromNullityDict Null Dict Dict = Dict
fromNullityDict NotNull Dict Dict = Dict


nullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Dict (ConstrainDBType c) ('Spec labels necessity a)
  -> Dict (ConstrainDBType c) ('Spec labels necessity (Nullify a))
nullifier SSpec {} dict = case dbTypeDict dict of
  Dict -> case dbTypeNullity dict of
    Null -> Dict
    NotNull -> Dict


unnullifier :: ()
  => SSpec ('Spec labels necessity a)
  -> Dict (ConstrainDBType c) ('Spec labels necessity (Nullify a))
  -> Dict (ConstrainDBType c) ('Spec labels necessity a)
unnullifier SSpec {exprable, nullity} dict = case dbTypeDict dict of
  Dict -> case nullity of
    Null -> Dict
    NotNull -> case dbTypeNullity dict of
      Null -> fromNullityDict nullity exprable Dict
