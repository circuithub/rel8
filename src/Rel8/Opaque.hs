{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Opaque
  ( Opaque
  )
where

-- base
import Prelude ()


type Opaque :: k
data family Opaque :: k
