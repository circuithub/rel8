{-# language GADTs #-}

module Rel8.Context ( Context(..) ) where


data Context where
  Haskell :: Context
  SQL :: m -> Context
  ToNull :: Context -> Context
  Schema :: Context
  Shape :: Context
