{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}

{-# options_ghc -O0 #-}

module Rel8.Generic.Rel8able.Test
  ( module Rel8.Generic.Rel8able.Test
  )
where

-- aeson
import Data.Aeson ( Value(..) )
import qualified Data.Aeson.KeyMap as Aeson

-- base
import Data.Fixed ( Fixed ( MkFixed ), E2 )
import Data.Int ( Int16, Int32, Int64 )
import Data.Functor.Identity ( Identity(..) )
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics ( Generic )
import Prelude
import Control.Applicative ( liftA3 )

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LB

-- case-insensitive
import Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- containers
import qualified Data.Map as Map

-- hedgehog
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- rel8
import Rel8 (
  Column,
  DBType,
  Expr,
  HADT,
  HEither,
  HKD,
  HList,
  HMaybe,
  HNonEmpty,
  HThese,
  KRel8able,
  Lift,
  Name,
  QualifiedName,
  Rel8able,
  Result,
  TableSchema (TableSchema),
  ToExprs,
  namesFromLabels,
  namesFromLabelsWith,
 )
import qualified Rel8

-- scientific
import Data.Scientific ( Scientific, fromFloatDigits )

-- time
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime, secondsToNominalDiffTime)
import Data.Time.LocalTime
  ( CalendarDiffTime (CalendarDiffTime)
  , LocalTime(..)
  , TimeOfDay(..)
  )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as LT

-- these
import Data.These

-- uuid
import Data.UUID ( UUID )
import qualified Data.UUID as UUID

-- vector
import qualified Data.Vector as Vector


makeSchema :: forall f. Rel8able f => QualifiedName -> TableSchema (f Name)
makeSchema name = TableSchema
  { name = name
  , columns = namesFromLabels @(f Name)
  }


data TableDuplicate f = TableDuplicate
  { foo :: TablePair f
  , bar :: TablePair f
  }
  deriving stock Generic
  deriving anyclass Rel8able

tableDuplicate :: TableSchema (TableDuplicate Name)
tableDuplicate = TableSchema
  { name = "tableDuplicate"
  , columns = namesFromLabelsWith NonEmpty.last
  }


data TableTest f = TableTest
  { foo :: Column f Bool
  , bar :: Column f (Maybe Bool)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableTest f)
deriving stock instance f ~ Result => Eq (TableTest f)
deriving stock instance f ~ Result => Ord (TableTest f)

tableTest :: TableSchema (TableTest Name)
tableTest = makeSchema "tableTest"

genTableTest :: Hedgehog.MonadGen m => m (TableTest Result)
genTableTest = TableTest <$> Gen.bool <*> Gen.maybe Gen.bool


data TablePair f = TablePair
  { foo :: Column f Bool
  , bars :: (Column f Text, Column f Text)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TablePair f)
deriving stock instance f ~ Result => Eq (TablePair f)
deriving stock instance f ~ Result => Ord (TablePair f)

tablePair :: TableSchema (TablePair Name)
tablePair = makeSchema "tablePair"

genTablePair :: Hedgehog.MonadGen m => m (TablePair Result)
genTablePair = TablePair
  <$> Gen.bool
  <*> liftA2 (,) (Gen.text (Range.linear 0 10) Gen.alphaNum) (Gen.text (Range.linear 0 10) Gen.alphaNum)


data TableMaybe f = TableMaybe
  { foo :: Column f [Maybe Bool]
  , bars :: HMaybe f (TablePair f, TablePair f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableMaybe f)
deriving stock instance f ~ Result => Eq (TableMaybe f)
deriving stock instance f ~ Result => Ord (TableMaybe f)

tableMaybe :: TableSchema (TableMaybe Name)
tableMaybe = makeSchema "tableMaybe"

genTableMaybe :: Hedgehog.MonadGen m => m (TableMaybe Result)
genTableMaybe = TableMaybe
  <$> Gen.list (Range.linear 0 10) (Gen.maybe Gen.bool)
  <*> Gen.maybe (liftA2 (,) genTablePair genTablePair)


data TableEither f = TableEither
  { foo :: Column f Bool
  , bars :: HEither f (HMaybe f (TablePair f, TablePair f)) (Column f Char)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableEither f)
deriving stock instance f ~ Result => Eq (TableEither f)
deriving stock instance f ~ Result => Ord (TableEither f)

tableEither :: TableSchema (TableEither Name)
tableEither = makeSchema "tableEither"

genTableEither :: Hedgehog.MonadGen m => m (TableEither Result)
genTableEither = TableEither
  <$> Gen.bool
  <*> Gen.either (Gen.maybe $ liftA2 (,) genTablePair genTablePair) Gen.alphaNum


data TableThese f = TableThese
  { foo :: Column f Bool
  , bars :: HThese f (TableMaybe f) (TableEither f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableThese f)
deriving stock instance f ~ Result => Eq (TableThese f)
deriving stock instance f ~ Result => Ord (TableThese f)

tableThese :: TableSchema (TableThese Name)
tableThese = makeSchema "tableThese"

genTableThese :: Hedgehog.MonadGen m => m (TableThese Result)
genTableThese = TableThese
  <$> Gen.bool
  <*> Gen.choice
    [ This <$> genTableMaybe
    , That <$> genTableEither
    , These <$> genTableMaybe <*> genTableEither
    ]


data TableList f = TableList
  { foo :: Column f Bool
  , bars :: HList f (TableThese f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableList f)
deriving stock instance f ~ Result => Eq (TableList f)
deriving stock instance f ~ Result => Ord (TableList f)

tableList :: TableSchema (TableList Name)
tableList = makeSchema "tableList"

genTableList :: Hedgehog.MonadGen m => m (TableList Result)
genTableList = TableList
  <$> Gen.bool
  <*> Gen.list (Range.linear 0 10) genTableThese


data TableNonEmpty f = TableNonEmpty
  { foo :: Column f Bool
  , bars :: HNonEmpty f (TableList f, TableMaybe f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableNonEmpty f)
deriving stock instance f ~ Result => Eq (TableNonEmpty f)
deriving stock instance f ~ Result => Ord (TableNonEmpty f)

tableNonEmpty :: TableSchema (TableNonEmpty Name)
tableNonEmpty = makeSchema "tableNonEmpty"

genTableNonEmpty :: Hedgehog.MonadGen m => m (TableNonEmpty Result)
genTableNonEmpty = TableNonEmpty
  <$> Gen.bool
  <*> Gen.nonEmpty (Range.linear 0 10) (liftA2 (,) genTableList genTableMaybe)


data TableNest f = TableNest
  { foo :: Column f Bool
  , bars :: HList f (HMaybe f (TablePair f))
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableNest f)
deriving stock instance f ~ Result => Eq (TableNest f)
deriving stock instance f ~ Result => Ord (TableNest f)

tableNest :: TableSchema (TableNest Name)
tableNest = makeSchema "tableNest"

genTableNest :: Hedgehog.MonadGen m => m (TableNest Result)
genTableNest = TableNest
  <$> Gen.bool
  <*> Gen.list (Range.linear 0 10) (Gen.maybe genTablePair)


data S3Object = S3Object
  { bucketName :: Text
  , objectKey :: Text
  }
  deriving stock (Generic, Show, Eq, Ord)


instance x ~ HKD S3Object Expr => ToExprs x S3Object


data HKDSum = HKDSumA Text | HKDSumB Bool Char | HKDSumC
  deriving stock (Generic, Show, Eq, Ord)


instance x ~ HKD HKDSum Expr => ToExprs x HKDSum

genHKDSum :: Hedgehog.MonadGen m => m HKDSum
genHKDSum = Gen.choice
  [ HKDSumA <$> Gen.text (Range.linear 0 10) Gen.alpha
  , HKDSumB <$> Gen.bool <*> Gen.alpha
  , pure HKDSumC
  ]

data HKDTest f = HKDTest
  { s3Object :: Lift f S3Object
  , hkdSum :: Lift f HKDSum
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (HKDTest f)
deriving stock instance f ~ Result => Eq (HKDTest f)
deriving stock instance f ~ Result => Ord (HKDTest f)

genHKDTest :: Hedgehog.MonadGen m => m (HKDTest Result)
genHKDTest = HKDTest
  <$> liftA2 S3Object (Gen.text (Range.linear 0 10) Gen.alpha) (Gen.text (Range.linear 0 10) Gen.alpha)
  <*> genHKDSum

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
deriving stock instance f ~ Result => Show (NonRecord f)
deriving stock instance f ~ Result => Eq (NonRecord f)
deriving stock instance f ~ Result => Ord (NonRecord f)

nonRecord :: TableSchema (NonRecord Name)
nonRecord = makeSchema "nonRecord"

genNonRecord :: Hedgehog.MonadGen m => m (NonRecord Result)
genNonRecord = NonRecord
  <$> Gen.bool
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha
  <*> Gen.alpha


data TableSum f
  = TableSumA (Column f Bool) (Column f Text)
  | TableSumB
  | TableSumC (Column f Text)
  deriving stock Generic
deriving stock instance f ~ Result => Show (TableSum f)
deriving stock instance f ~ Result => Eq (TableSum f)
deriving stock instance f ~ Result => Ord (TableSum f)


genTableSum :: Hedgehog.MonadGen m => m (HADT Result TableSum)
genTableSum = Gen.choice
  [ TableSumA <$> Gen.bool <*> Gen.text (Range.linear 0 10) Gen.alpha
  , pure TableSumB
  , TableSumC <$> Gen.text (Range.linear 0 10) Gen.alpha
  ]


data BarbieSum f
  = BarbieSumA (f Bool) (f Text)
  | BarbieSumB
  | BarbieSumC (f Text)
  deriving stock Generic
deriving stock instance f ~ Result => Show (BarbieSum f)
deriving stock instance f ~ Result => Eq (BarbieSum f)
deriving stock instance f ~ Result => Ord (BarbieSum f)


genBarbieSum :: Hedgehog.MonadGen m => m (BarbieSum Result)
genBarbieSum = Gen.choice
  [ BarbieSumA <$> fmap Identity Gen.bool <*> fmap Identity (Gen.text (Range.linear 0 10) Gen.alpha)
  , pure BarbieSumB
  , BarbieSumC <$> fmap Identity (Gen.text (Range.linear 0 10) Gen.alpha)
  ]


data TableProduct f = TableProduct
  { sum :: HADT f BarbieSum
  , list :: TableList f
  , foos :: HList f (HADT f TableSum, Lift f HKDSum, HKDTest f)
  }
  deriving stock Generic
  deriving anyclass Rel8able
deriving stock instance f ~ Result => Show (TableProduct f)
deriving stock instance f ~ Result => Eq (TableProduct f)
deriving stock instance f ~ Result => Ord (TableProduct f)

tableProduct :: TableSchema (TableProduct Name)
tableProduct = makeSchema "tableProduct"

genTableProduct :: Hedgehog.MonadGen m => m (TableProduct Result)
genTableProduct = TableProduct
  <$> genBarbieSum
  <*> genTableList
  <*> Gen.list (Range.linear 0 10) (liftA3 (,,) genTableSum genHKDSum genHKDTest)

-- tableProduct :: TableProduct Name
-- tableProduct = makeSchema "tableProduct"

-- genTableProduct :: Hedgehog.MonadGen m => m (TableProduct Result)
-- genTableProduct = TableProduct
--   <$> Gen.choice
--     [ BarbieSumA <$> Gen.bool <*> Gen.text (Range.linear 0 10) Gen.alpha
--     , BarbieSumB
--     , BarbieSumC <$> Gen.text (Range.linear 0 10) Gen.alpha
--     ]
--   <*> genTableList
--   <*> Gen.list (Range.linear 0 10) (liftA3 (,,) genTableSum)

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


data TableType f = TableType
  { bool                :: Column f Bool
  , char                :: Column f Char
  , int16               :: Column f Int16
  , int32               :: Column f Int32
  , int64               :: Column f Int64
  , float               :: Column f Float
  , double              :: Column f Double
  , scientific          :: Column f Scientific
  , fixed               :: Column f (Fixed E2)
  , utctime             :: Column f UTCTime
  , day                 :: Column f Day
  , localtime           :: Column f LocalTime
  , timeofday           :: Column f TimeOfDay
  , calendardifftime    :: Column f CalendarDiffTime
  , text                :: Column f Text
  , lazytext            :: Column f LT.Text
  , citext              :: Column f (CI Text)
  , cilazytext          :: Column f (CI LT.Text)
  , bytestring          :: Column f ByteString
  , lazybytestring      :: Column f LB.ByteString
  , uuid                :: Column f UUID
  , value               :: Column f Value
  } deriving stock (Generic)
deriving anyclass instance Rel8able TableType
deriving stock instance f ~ Result => Show (TableType f)
deriving stock instance f ~ Result => Eq (TableType f)
-- deriving stock instance f ~ Result => Ord (TableType f)

tableType :: TableSchema (TableType Name)
tableType = makeSchema "tableType"

badTableType :: TableSchema (TableProduct Name)
badTableType = makeSchema "tableType"

genTableType :: Hedgehog.MonadGen m => m (TableType Result)
genTableType = do
  bool <- Gen.bool
  char <- Gen.alpha
  int16 <- Gen.int16 range
  int32 <- Gen.int32 range
  int64 <- Gen.int64 range
  float <- Gen.float linearFrac
  double <- Gen.double linearFrac
  scientific <- fromFloatDigits @Double <$> Gen.realFloat linearFrac
  utctime <- UTCTime <$> (toEnum <$> Gen.integral range) <*> fmap secondsToDiffTime (Gen.integral range)
  day <- toEnum <$> Gen.integral range
  localtime <- LocalTime <$> (toEnum <$> Gen.integral range) <*> timeOfDay
  timeofday <- timeOfDay
  text <- Gen.text range Gen.alpha
  lazytext <- LT.fromStrict <$> Gen.text range Gen.alpha
  citext <- CI.mk <$> Gen.text range Gen.alpha
  cilazytext <- CI.mk <$> LT.fromStrict <$> Gen.text range Gen.alpha
  bytestring <- Gen.bytes range
  lazybytestring <- LB.fromStrict <$> Gen.bytes range
  uuid <- UUID.fromWords <$> Gen.word32 range <*> Gen.word32 range <*> Gen.word32 range <*> Gen.word32 range
  fixed <- MkFixed <$> Gen.integral range
  value <- Gen.choice
    [ Object <$> Aeson.fromMapText <$> Map.fromList <$> Gen.list range (liftA2 (,) (Gen.text range Gen.alpha) (pure Null))
    , Array <$> Vector.fromList <$> Gen.list range (pure Null)
    , String <$> Gen.text range Gen.alpha
    , Number <$> fromFloatDigits @Double <$> Gen.realFloat linearFrac
    , Bool <$> Gen.bool
    , pure Null
    ]
  calendardifftime <- CalendarDiffTime <$> Gen.integral range <*> (secondsToNominalDiffTime <$> Gen.realFrac_ linearFrac)
  pure TableType {..}
  where
    timeOfDay :: Hedgehog.MonadGen m => m TimeOfDay
    timeOfDay = TimeOfDay <$> Gen.integral range <*> Gen.integral range <*> Gen.realFrac_ linearFrac

    range :: Integral a => Range.Range a
    range = Range.linear 0 10

    linearFrac :: (Fractional a, Ord a) => Range.Range a
    linearFrac = Range.linearFrac 0 10

data TableNumeric f = TableNumeric
  { foo :: Column f (Fixed E2)
  } deriving stock (Generic)
deriving anyclass instance Rel8able TableNumeric
deriving stock instance f ~ Result => Show (TableNumeric f)
deriving stock instance f ~ Result => Eq (TableNumeric f)

tableNumeric :: TableSchema (TableNumeric Name)
tableNumeric = makeSchema "tableNumeric"


data TableChar f = TableChar
  { foo :: Column f Char
  } deriving stock (Generic)
deriving anyclass instance Rel8able TableChar
deriving stock instance f ~ Result => Show (TableChar f)
deriving stock instance f ~ Result => Eq (TableChar f)

tableChar :: TableSchema (TableChar Name)
tableChar = makeSchema "tableChar"


