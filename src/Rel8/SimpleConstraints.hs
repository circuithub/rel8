{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.SimpleConstraints ( Selects, IsTableIn ) where

import Rel8.Column
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.Table


-- | @Selects m schema row@ says that in the monad @m@, the schema definition
-- @schema@ can be @SELECT@ed into the Haskell type @row@.
class
  ( Context row ~ Expr
  , Context schema ~ ColumnSchema
  , MapTable From schema ~ row
  , Recontextualise schema From
  , Table schema
  , Table row
  ) => Selects schema row


instance
  {-# overlapping #-}
  ( Context row ~ Expr
  , Context schema ~ ColumnSchema
  , MapTable From schema ~ row
  , Recontextualise schema From
  , Table row
  , Table schema
  ) => Selects schema row


-- | Makes sure that a given table (@a@) contains expressions compatible with
-- the monad @m@. This type class is essentially a scoping check.
class
  ( Table a
  , Context a ~ Expr
  ) => a `IsTableIn` m
