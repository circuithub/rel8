{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}

module Rel8.Schema.Table
  ( TableSchema(..)
  )
where

-- base
import Prelude


data TableSchema names = TableSchema
  { name :: String
  , schema :: Maybe String
  , columns :: names
  }
  deriving stock Functor
