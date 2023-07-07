{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Type.Monoid (
  DBMonoid (memptyExpr),
)
where

-- base
import Data.Kind (Constraint, Type)
import Prelude hiding (null)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

-- case-insensitive
import Data.CaseInsensitive (CI)

-- rel8
import {-# SOURCE #-} Rel8.Expr (Expr)
import Rel8.Expr.Array (sempty)
import Rel8.Expr.Serialize (litExpr)
import Rel8.Schema.Null (Sql)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Semigroup (DBSemigroup)

-- text
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)

-- time
import Data.Time.LocalTime (CalendarDiffTime (CalendarDiffTime))


{- | The class of 'Rel8.DBType's that form a semigroup. This class is purely a
Rel8 concept, and exists to mirror the 'Monoid' class.
-}
type DBMonoid :: Type -> Constraint
class DBSemigroup a => DBMonoid a where
  -- The identity for '<>.'
  memptyExpr :: Expr a


instance Sql DBType a => DBMonoid [a] where
  memptyExpr = sempty typeInformation


instance DBMonoid CalendarDiffTime where
  memptyExpr = litExpr (CalendarDiffTime 0 0)


instance DBMonoid Text where
  memptyExpr = litExpr ""


instance DBMonoid Lazy.Text where
  memptyExpr = litExpr ""


instance DBMonoid (CI Text) where
  memptyExpr = litExpr ""


instance DBMonoid (CI Lazy.Text) where
  memptyExpr = litExpr ""


instance DBMonoid ByteString where
  memptyExpr = litExpr ""


instance DBMonoid Lazy.ByteString where
  memptyExpr = litExpr ""
