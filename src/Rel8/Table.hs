{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Table ( Table(..) ) where

import Data.Kind (Constraint, Type)
import Rel8.Context ( KContext, Context )
import Rel8.HTable ( HTable(..) )

type Table :: (Type -> Type) -> Type -> Constraint
class HTable (Columns t) => Table context t | t -> context where
  type Columns t :: KContext -> Type

  toColumns :: t -> Columns t (Context context)
  fromColumns :: Columns t (Context context) -> t
