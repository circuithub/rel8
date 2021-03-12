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
import Rel8.Kind.Necessity
import Rel8.Kind.Nullability
import Rel8.Schema.Column
import Rel8.Schema.Generic

-- text
import Data.Text ( Text )


data Table f = Table
  { foo :: Column f 'Required 'NonNullable Bool
  , bar :: Column f 'Required 'NonNullable Text
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TablePair f = TablePair
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: (Column f 'Required 'NonNullable Text, Column f 'Required 'NonNullable Text)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableMaybe f = TableMaybe
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: HMaybe f (TablePair f, TablePair f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableEither f = TableEither
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: HEither f (HMaybe f (TablePair f, TablePair f)) (Column f 'Required 'Nullable Char)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableThese f = TableThese
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: HThese f (TableMaybe f) (TableEither f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableList f = TableList
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: HList f (TableThese f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableNonEmpty f = TableNonEmpty
  { foo :: Column f 'Required 'NonNullable Bool
  , bars :: HNonEmpty f (TableList f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
