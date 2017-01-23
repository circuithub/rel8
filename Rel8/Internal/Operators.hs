{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rel8.Internal.Operators where

import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import Prelude hiding (not)
import Rel8.Internal.DBType
import Rel8.Internal.Expr

--------------------------------------------------------------------------------
infix 4 ==. , ?=. , <. , <=. , >. , >=.
infixr 2 ||.,  &&.
infixl 7 *.

--------------------------------------------------------------------------------
class ToNullable a (Maybe Bool) => DBBool a where
  not :: Expr a -> Expr a
  (&&.) :: Expr a -> Expr a -> Expr a
  (||.) :: Expr a -> Expr a -> Expr a

instance DBBool Bool where
  not (Expr a) = Expr (O.UnExpr O.OpNot a)
  Expr a &&. Expr b = Expr (O.BinExpr O.OpAnd a b)
  Expr a ||. Expr b = Expr (O.BinExpr O.OpOr a b)

instance DBBool (Maybe Bool) where
  not = unsafeCoerceExpr . not . unsafeCoerceExpr @Bool
  a &&. b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a &&. unsafeCoerceExpr @Bool b)
  a ||. b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a ||. unsafeCoerceExpr @Bool b)


--------------------------------------------------------------------------------
-- | The class of types that can be compared for equality within the database.
class DBType a => DBEq a where
  (==.) :: Expr a -> Expr a -> Expr Bool
  Expr a ==. Expr b = Expr (O.BinExpr (O.:==) a b)

  (?=.) :: Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe Bool)
  a ?=. b = toNullable (unsafeCoerceExpr @a a ==. unsafeCoerceExpr @a b)

instance DBEq Bool where
instance DBEq Char where
instance DBEq Double where
instance DBEq Float where
instance DBEq Int16 where
instance DBEq Int32 where
instance DBEq Int64 where
instance DBEq Text where
instance DBEq UTCTime where


--------------------------------------------------------------------------------
class DBEq a => DBOrd a where
  -- | The PostgreSQL @<@ operator.
  (<.) :: Expr a -> Expr a -> Expr Bool
  a <. b = not (a >=. b)

  -- | The PostgreSQL @<=@ operator.
  (<=.) :: Expr a -> Expr a -> Expr Bool
  a <=. b = not (a >. b)

  -- | The PostgreSQL @>@ operator.
  (>.) :: Expr a -> Expr a -> Expr Bool
  a >. b = not (a <=. b)

  -- | The PostgreSQL @>@ operator.
  (>=.) :: Expr a -> Expr a -> Expr Bool
  a >=. b = not (a <. b)

instance DBOrd Bool where
instance DBOrd Char where
instance DBOrd Double where
instance DBOrd Float where
instance DBOrd Int16 where
instance DBOrd Int32 where
instance DBOrd Int64 where
instance DBOrd Text where
instance DBOrd UTCTime where


--------------------------------------------------------------------------------
class DBType a => DBNum a where
  (*.) :: Expr a -> Expr a -> Expr a
  a *. b = columnToExpr (O.binOp (O.:*) (exprToColumn a) (exprToColumn b))

instance DBNum Double where
instance DBNum Float where
instance DBNum Int16 where
instance DBNum Int32 where
instance DBNum Int64 where
