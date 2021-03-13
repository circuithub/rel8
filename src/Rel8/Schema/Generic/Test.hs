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
import Rel8.Kind.Blueprint
import Rel8.Kind.Emptiability
import Rel8.Kind.Necessity
import Rel8.Kind.Nullability
import Rel8.Schema.Column
import Rel8.Schema.Generic

-- text
import Data.Text ( Text )


data Table f = Table
  { foo :: Column f 'Required 'NonNullable ('Scalar Bool)
  , bar :: Column f 'Required 'NonNullable ('Scalar Text)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TablePair f = TablePair
  { foo :: Column f 'Optional 'NonNullable ('Scalar Bool)
  , bars :: (Column f 'Required 'NonNullable ('Scalar Text), Column f 'Required 'NonNullable ('Scalar Text))
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableMaybe f = TableMaybe
  { foo :: Column f 'Required 'NonNullable ('Vector 'Emptiable 'Nullable ('Scalar Bool))
  , bars :: HMaybe f (TablePair f, TablePair f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableEither f = TableEither
  { foo :: Column f 'Required 'NonNullable ('Scalar Bool)
  , bars :: HEither f (HMaybe f (TablePair f, TablePair f)) (Column f 'Required 'Nullable ('Scalar Char))
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableThese f = TableThese
  { foo :: Column f 'Required 'NonNullable ('Scalar Bool)
  , bars :: HThese f (TableMaybe f) (TableEither f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableList f = TableList
  { foo :: Column f 'Required 'NonNullable ('Scalar Bool)
  , bars :: HList f (TableThese f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableNonEmpty f = TableNonEmpty
  { foo :: Column f 'Required 'NonNullable ('Scalar Bool)
  , bars :: HNonEmpty f (TableList f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
