{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

{-# options_ghc -O0 #-}

module Rel8.Generic.Rel8able.Test
  ( module Rel8.Generic.Rel8able.Test
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- higgledy
import Data.Generic.HKD ( HKD )

-- rel8
import Rel8

-- text
import Data.Text ( Text )


data Table f = Table
  { foo :: Column f Bool
  , bar :: Column f (Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TablePair f = TablePair
  { foo :: Default f Bool
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
  , bars :: HNonEmpty f (TableList f, TableMaybe f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data S3Object = S3Object
  { bucketName :: Text
  , objectKey :: Text
  }
  deriving stock Generic


deriving via HKDT S3Object
  instance x ~ HKD S3Object Expr => ToExprs x S3Object


newtype HKDTest f = HKDTest
  { s3Object :: Lift f S3Object
  } 
  deriving stock Generic
  deriving anyclass Rel8able


data NonRecord f = NonRecord
  (Column f Bool)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  (Column f Char)
  deriving stock Generic
  deriving anyclass Rel8able


data TableSum f
  = TableSumA (Column f Bool) (Column f Text)
  | TableSumB
  | TableSumC (Column f Text)
  deriving stock Generic
  deriving anyclass Rel8able


data TableProduct f = TableProduct
  { sum :: HADT f TableSum
  , list :: TableList f
  }
  deriving stock Generic
  deriving anyclass Rel8able
