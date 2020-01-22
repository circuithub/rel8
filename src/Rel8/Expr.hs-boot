{-# language DataKinds #-}
{-# language KindSignatures #-}

module Rel8.Expr where

import Data.Kind
import Rel8.Null


data Expr ( m :: * -> * ) ( a :: Null Type )
