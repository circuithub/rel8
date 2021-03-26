{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Schema.Nullability
  ( module Rel8.Schema.Nullability.Internal
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Nullability.Internal


instance {-# OVERLAPPING #-} (c Opaque, Sql c Opaque) => Sql c Opaque
