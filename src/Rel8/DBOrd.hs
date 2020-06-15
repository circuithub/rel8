{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.DBOrd where

import Data.Int
import Data.Kind
import Data.Text


{-| Database types that can be compared for order in queries. -}
class DBOrd ( a :: Type ) where


instance DBOrd String


instance DBOrd Int32


instance DBOrd Int64


instance DBOrd Text


instance DBOrd Bool


instance DBOrd a => DBOrd ( Maybe a ) where
