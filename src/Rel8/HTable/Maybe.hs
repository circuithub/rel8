{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language TypeFamilies #-}

module Rel8.HTable.Maybe
  ( HMaybeTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Rel8.HTable ( HTable(..) )
import Rel8.HTable.Identity ( HIdentity )
import Rel8.Type.Tag (MaybeTag)


data HMaybeTable table context = HMaybeTable
  { htag :: HIdentity (Maybe MaybeTag) context
  , hjust :: table context
  }
  deriving stock Generic
  deriving anyclass HTable
