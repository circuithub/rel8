{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NoStarIsType #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Decimal
  ( PowerOf10
  , resolution
  )
where

-- base
import Data.Fixed (E0, E1, E2, E3, E6, E9, E12, HasResolution)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (type (==))
import Data.Type.Ord (type (<?))
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage ((:<>:), ShowType, Text), TypeError)
import GHC.TypeNats (KnownNat, Nat, type (+), type (-), type (*), Div, natVal)
import Numeric.Natural (Natural)
import Prelude


type PowerOf10 :: a -> Constraint
class (HasResolution n, KnownNat (Log n)) => PowerOf10 n where
  type Log n :: Nat


instance (KnownNat n, KnownNat (Log n), IsPowerOf10 n) => PowerOf10 n where
  type Log n = Log10 n


instance PowerOf10 E0 where
  type Log E0 = 0


instance PowerOf10 E1 where
  type Log E1 = 1


instance PowerOf10 E2 where
  type Log E2 = 2


instance PowerOf10 E3 where
  type Log E3 = 3


instance PowerOf10 E6 where
  type Log E6 = 6


instance PowerOf10 E9 where
  type Log E9 = 9


instance PowerOf10 E12 where
  type Log E12 = 12


resolution :: forall n. PowerOf10 n => Natural
resolution = natVal (Proxy @(Log n))


type Exp10 :: Nat -> Nat
type Exp10 n = Exp10' 1 n


type Exp10' :: Nat -> Nat -> Nat
type family Exp10' x n where
  Exp10' x 0 = x
  Exp10' x n = Exp10' (x * 10) (n - 1)


type Log10 :: Nat -> Nat
type Log10 n = Log10' (n <? 10) n


type Log10' :: Bool -> Nat -> Nat
type family Log10' bool n where
  Log10' 'True _n = 0
  Log10' 'False n = 1 + Log10 (Div n 10)


type IsPowerOf10 :: Nat -> Constraint
type IsPowerOf10 n = IsPowerOf10' (Exp10 (Log10 n) == n) n


type IsPowerOf10' :: Bool -> Nat -> Constraint
type family IsPowerOf10' bool n where
  IsPowerOf10' 'True _n = ()
  IsPowerOf10' 'False n =
    TypeError ('ShowType n ':<>: 'Text " is not a power of 10")