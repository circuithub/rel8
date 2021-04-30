{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.HKD
  ( HKDT(..)
  )
where

-- base
import Data.Functor.Identity ( Identity(..), runIdentity )
import Data.Kind ( Type )
import GHC.Generics ( Rep )
import Prelude

-- higgledy
import Data.Generic.HKD ( Construct, HKD(..), construct, deconstruct )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Generic.HKD ( GTable )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )


type HKDT :: Type -> Type
newtype HKDT a = HKDT
  { unHKDT :: a
  }


instance (GTable (Rep a), Construct Identity a, x ~ HKD a Expr) =>
  ToExprs x (HKDT a)
 where
  toResult = toColumns . deconstruct @Identity . unHKDT
  fromResult = HKDT . runIdentity . construct . fromColumns


type instance FromExprs (HKD a Expr) = a
