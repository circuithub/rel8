{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Info ( Info(..), HasInfo(..), Nullify ) where

-- base
import Data.Kind ( Type )

-- rel8
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType ( DatabaseType )


data Info :: Type -> Type where
  NotNull :: Nullify a ~ Maybe a => DatabaseType a -> Info a
  Null :: DatabaseType a -> Info (Maybe a)


type family Nullify (a :: Type) :: Type where
  Nullify (Maybe a) = Maybe a
  Nullify a         = Maybe a


class HasInfo a where
  info :: Info a


instance {-# overlapping #-} DBType a => HasInfo (Maybe a) where
  info = Null typeInformation


instance (DBType a, Nullify a ~ Maybe a) => HasInfo a where
  info = NotNull typeInformation
