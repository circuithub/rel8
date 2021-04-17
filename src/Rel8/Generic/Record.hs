{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Record
  ( Record(..)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
import GHC.TypeLits
import Prelude hiding ( Show )


type Recordize :: (Type -> Type) -> Type -> Type
type family Recordize rep where
  Recordize (M1 D meta rep) = M1 D meta (Recordize rep)
  Recordize (l :+: r) = Recordize l :+: Recordize r
  Recordize (M1 C ('MetaCons name fixity 'False) rep) =
    M1 C ('MetaCons name fixity 'True) (Snd (Count 0 rep))
  Recordize rep = rep


type Count :: Nat -> (Type -> Type) -> (Nat, Type -> Type)
type family Count n rep where
  Count n (M1 S ('MetaSel _selector su ss ds) rep) =
    '(n + 1, M1 S ('MetaSel ('Just (Show (n + 1))) su ss ds) rep)
  Count n (a :*: b) = CountHelper1 (Count n a) b
  Count n rep = '(n, rep)


type CountHelper1 :: (Nat, Type -> Type) -> (Type -> Type) -> (Nat, Type -> Type)
type family CountHelper1 tuple b where
  CountHelper1 '(n, a) b = CountHelper2 a (Count n b)


type CountHelper2 :: (Type -> Type) -> (Nat, Type -> Type) -> (Nat, Type -> Type)
type family CountHelper2 a tuple where
  CountHelper2 a '(n, b) = '(n, a :*: b)


type Show :: Nat -> Symbol
type Show n =
  AppendSymbol "_" (AppendSymbol (Show' (Div n 10)) (ShowDigit (Mod n 10)))


type Show' :: Nat -> Symbol
type family Show' n where
  Show' 0 = ""
  Show' n = AppendSymbol (Show' (Div n 10)) (ShowDigit (Mod n 10))


type ShowDigit :: Nat -> Symbol
type family ShowDigit n where
  ShowDigit 0 = "0"
  ShowDigit 1 = "1"
  ShowDigit 2 = "2"
  ShowDigit 3 = "3"
  ShowDigit 4 = "4"
  ShowDigit 5 = "5"
  ShowDigit 6 = "6"
  ShowDigit 7 = "7"
  ShowDigit 8 = "8"
  ShowDigit 9 = "9"


type Snd :: (a, b) -> b
type family Snd tuple where
  Snd '(_a, b) = b


type Recordizable :: (Type -> Type) -> Constraint
class Recordizable rep where
  recordize :: rep x -> Recordize rep x
  unrecordize :: Recordize rep x -> rep x


instance Recordizable rep => Recordizable (M1 D meta rep) where
  recordize (M1 a) = M1 (recordize a)
  unrecordize (M1 a) = M1 (unrecordize a)


instance (Recordizable l, Recordizable r) => Recordizable (l :+: r) where
  recordize (L1 a) = L1 (recordize a)
  recordize (R1 a) = R1 (recordize a)
  unrecordize (L1 a) = L1 (unrecordize a)
  unrecordize (R1 a) = R1 (unrecordize a)


instance Countable 0 rep =>
  Recordizable (M1 C ('MetaCons name fixity 'False) rep)
 where
  recordize (M1 a) = M1 (count @0 a)
  unrecordize (M1 a) = M1 (uncount @0 a)


instance {-# OVERLAPPABLE #-} Recordize rep ~ rep => Recordizable rep where
  recordize = id
  unrecordize = id


type Countable :: Nat -> (Type -> Type) -> Constraint
class Countable n rep where
  count :: rep x -> Snd (Count n rep) x
  uncount :: Snd (Count n rep) x -> rep x


instance Countable n (M1 S ('MetaSel selector su ss ds) rep) where
  count (M1 a) = M1 a
  uncount (M1 a) = M1 a


instance
  ( Countable n a, Countable n' b
  , '(n', a') ~ Count n a
  , Snd (CountHelper2 a' (Count n' b)) ~ (a' :*: Snd (Count n' b))
  )
  => Countable n (a :*: b)
 where
  count (a :*: b) = count @n a :*: count @n' b
  uncount (a :*: b) = uncount @n a :*: uncount @n' b


instance {-# OVERLAPPABLE #-} Snd (Count n rep) ~ rep => Countable n rep where
  count = id
  uncount = id


newtype Record a = Record
  { unrecord :: a
  }


instance (Generic a, Recordizable (Rep a)) => Generic (Record a) where
  type Rep (Record a) = Recordize (Rep a)

  from (Record a) = recordize (from a)
  to = Record . to . unrecordize
