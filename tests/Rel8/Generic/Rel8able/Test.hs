{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -O0 #-}

module Rel8.Generic.Rel8able.Test
  ( module Rel8.Generic.Rel8able.Test
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8
import Rel8.TH ( deriveRel8able )

-- text
import Data.Text ( Text )


data TableTest f = TableTest
  { foo :: Column f Bool
  , bar :: Column f (Maybe Bool)
  }


deriveRel8able ''TableTest


data TablePair f = TablePair
  { foo :: Column f Bool
  , bars :: (Column f Text, Column f Text)
  }


deriveRel8able ''TablePair


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


data TableNest f = TableNest
  { foo :: Column f Bool
  , bars :: HList f (HMaybe f (TablePair f))
  }
  deriving stock Generic
  deriving anyclass Rel8able


data S3Object = S3Object
  { bucketName :: Text
  , objectKey :: Text
  }
  deriving stock Generic


instance x ~ HKD S3Object Expr => ToExprs x S3Object


data HKDSum = HKDSumA Text | HKDSumB Bool Char | HKDSumC
  deriving stock Generic


instance x ~ HKD HKDSum Expr => ToExprs x HKDSum


data HKDTest f = HKDTest
  { s3Object :: Lift f S3Object
  , hkdSum :: Lift f HKDSum
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


data BarbieSum f
  = BarbieSumA (f Bool) (f Text)
  | BarbieSumB
  | BarbieSumC (f Text)
  deriving stock Generic


data TableProduct f = TableProduct
  { sum :: HADT f BarbieSum
  , list :: TableList f
  , foos :: HList f (HADT f TableSum, Lift f HKDSum, HKDTest f)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data TableTestB f = TableTestB
  { foo :: f Bool
  , bar :: f (Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass Rel8able


data NestedTableTestB f = NestedTableTestB
  { foo :: f Bool
  , bar :: f (Maybe Bool)
  , baz :: Column f Char
  , nest :: TableTestB f
  }
  deriving stock Generic
  deriving anyclass Rel8able


newtype IdRecord a f = IdRecord { recordId :: Column f a }
  deriving stock Generic


instance DBType a => Rel8able (IdRecord a)


type Nest :: KRel8able -> KRel8able -> KRel8able
data Nest t u f = Nest
  { foo :: t f
  , bar :: u f
  }
  deriving stock Generic
  deriving anyclass Rel8able
