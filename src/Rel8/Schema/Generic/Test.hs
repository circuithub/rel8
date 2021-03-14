{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}

module Rel8.Schema.Generic.Test
  ( module Rel8.Schema.Generic.Test
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Schema.Column
import Rel8.Schema.Generic

-- text
import Data.Text ( Text )


data Table f = Table
  { foo :: Column f Bool
  , bar :: Column f (Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TablePair f = TablePair
  { foo :: Column f (Default Bool)
  , bars :: (Column f Text, Column f Text)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableMaybe f = TableMaybe
  { foo :: Column f [Maybe Bool]
  , bars :: HMaybe f (TablePair f, TablePair f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableEither f = TableEither
  { foo :: Column f Bool
  , bars :: HEither f (HMaybe f (TablePair f, TablePair f)) (Column f Char)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableThese f = TableThese
  { foo :: Column f Bool
  , bars :: HThese f (TableMaybe f) (TableEither f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableList f = TableList
  { foo :: Column f Bool
  , bars :: HList f (TableThese f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableNonEmpty f = TableNonEmpty
  { foo :: Column f Bool
  , bars :: HNonEmpty f (TableList f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
