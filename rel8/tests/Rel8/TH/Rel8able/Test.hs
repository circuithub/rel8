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

module Rel8.TH.Rel8able.Test where

-- base
import Data.Fixed ( Fixed ( MkFixed ), E2 )
import Prelude

-- rel8
import Rel8 (
  Column,
  HEither,
  HList,
  HMaybe,
  HNonEmpty,
  HThese, 
 )
import Rel8.TH

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

deriveRel8able ''TableTestB

data NestedTableTestB f = NestedTableTestB
  { foo :: f Bool
  , bar :: f (Maybe Bool)
  , baz :: Column f Char
  , nest :: TableTestB f
  }
  
deriveRel8able ''NestedTableTestB

data TableNumeric f = TableNumeric
  { foo :: Column f (Fixed E2)
  }
  
deriveRel8able ''TableNumeric


data TableChar f = TableChar
  { foo :: Column f Char
  } 
deriveRel8able ''TableChar


newtype IdRecord a f = IdRecord { recordId :: Column f a }

-- Our TH deriving code currently doesn't support type args other than f
-- deriveRel8able ''IdRecord
