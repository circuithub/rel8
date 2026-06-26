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
{-# LANGUAGE TemplateHaskell #-}
-- Maybe we want to drop this
module Rel8.Deriving.TH where
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

import Rel8.TH

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

data TableDuplicate f = TableDuplicate
  { foo :: TablePair f
  , bar :: TablePair f
  }

deriveRel8able ''TableDuplicate 

data TableMaybe f = TableMaybe
  { foo :: Column f [Maybe Bool]
  , bars :: HMaybe f (TablePair f, TablePair f)
  }
  
deriveRel8able ''TableMaybe

data TableEither f = TableEither
  { foo :: Column f Bool
  , bars :: HEither f (HMaybe f (TablePair f, TablePair f)) (Column f Char)
  }
  
deriveRel8able ''TableEither

data TableThese f = TableThese
  { foo :: Column f Bool
  , bars :: HThese f (TableMaybe f) (TableEither f)
  }
  
deriveRel8able ''TableThese


data TableList f = TableList
  { foo :: Column f Bool
  , bars :: HList f (TableThese f)
  }
  
deriveRel8able ''TableList


data TableNonEmpty f = TableNonEmpty
  { foo :: Column f Bool
  , bars :: HNonEmpty f (TableList f, TableMaybe f)
  }
  
deriveRel8able ''TableNonEmpty

data TableNest f = TableNest
  { foo :: Column f Bool
  , bars :: HList f (HMaybe f (TablePair f))
  }
  
deriveRel8able ''TableNest


data TableTestB f = TableTestB
  { foo :: f Bool
  , bar :: f (Maybe Bool)
  }
  
--deriveRel8able ''TableTestB

data NestedTableTestB f = NestedTableTestB
  { foo :: f Bool
  , bar :: f (Maybe Bool)
  , baz :: Column f Char
  , nest :: TableTestB f
  }
  
--deriveRel8able ''NestedTableTestB

newtype IdRecord a f = IdRecord { recordId :: Column f a }

--deriveRel8able ''IdRecord

data TableNumeric f = TableNumeric
  { foo :: Column f (Fixed E2)
  }
  
deriveRel8able ''TableNumeric


data TableChar f = TableChar
  { foo :: Column f Char
  } 
deriveRel8able ''TableChar
