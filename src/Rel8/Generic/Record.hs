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
  , GRecordable, GRecord, grecord, gunrecord
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( Generic, Rep, from, to
  , (:+:)( L1, R1 ), (:*:)( (:*:) ), M1( M1 )
  , Meta( MetaCons, MetaSel ), D, C, S
  )
import GHC.TypeLits ( type (+), AppendSymbol, Div, Mod, Nat, Symbol )
import Prelude hiding ( Show )


type GRecord :: (Type -> Type) -> Type -> Type
type family GRecord rep where
  GRecord (M1 D meta rep) = M1 D meta (GRecord rep)
  GRecord (l :+: r) = GRecord l :+: GRecord r
  GRecord (M1 C ('MetaCons name fixity 'False) rep) =
    M1 C ('MetaCons name fixity 'True) (Snd (Count 0 rep))
  GRecord rep = rep


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


type GRecordable :: (Type -> Type) -> Constraint
class GRecordable rep where
  grecord :: rep x -> GRecord rep x
  gunrecord :: GRecord rep x -> rep x


instance GRecordable rep => GRecordable (M1 D meta rep) where
  grecord (M1 a) = M1 (grecord a)
  gunrecord (M1 a) = M1 (gunrecord a)


instance (GRecordable l, GRecordable r) => GRecordable (l :+: r) where
  grecord (L1 a) = L1 (grecord a)
  grecord (R1 a) = R1 (grecord a)
  gunrecord (L1 a) = L1 (gunrecord a)
  gunrecord (R1 a) = R1 (gunrecord a)


instance Countable 0 rep =>
  GRecordable (M1 C ('MetaCons name fixity 'False) rep)
 where
  grecord (M1 a) = M1 (count @0 a)
  gunrecord (M1 a) = M1 (uncount @0 a)


instance {-# OVERLAPPABLE #-} GRecord rep ~ rep => GRecordable rep where
  grecord = id
  gunrecord = id


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


instance (Generic a, GRecordable (Rep a)) => Generic (Record a) where
  type Rep (Record a) = GRecord (Rep a)

  from (Record a) = grecord (from a)
  to = Record . to . gunrecord
