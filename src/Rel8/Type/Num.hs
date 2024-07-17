{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Num
  ( DBNum, DBIntegral, DBFractional, DBFloating
  )
where

-- base
import Data.Fixed (Fixed)
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Type ( DBType )
import Rel8.Type.Decimal (PowerOf10)
import Rel8.Type.Ord ( DBOrd )

-- scientific
import Data.Scientific ( Scientific )


-- | The class of database types that support the @+@, @*@, @-@ operators, and
-- the @abs@, @negate@, @sign@ functions.
type DBNum :: Type -> Constraint
class DBType a => DBNum a
instance DBNum Int16
instance DBNum Int32
instance DBNum Int64
instance PowerOf10 n => DBNum (Fixed n)
instance DBNum Float
instance DBNum Double
instance DBNum Scientific


-- | The class of database types that can be coerced to from integral
-- expressions. This is a Rel8 concept, and allows us to provide
-- 'Rel8.Expr.Num.fromIntegral'.
type DBIntegral :: Type -> Constraint
class (DBNum a, DBOrd a) => DBIntegral a
instance DBIntegral Int16
instance DBIntegral Int32
instance DBIntegral Int64


-- | The class of database types that support the @/@ operator.
type DBFractional :: Type -> Constraint
class DBNum a => DBFractional a
instance PowerOf10 n => DBFractional (Fixed n)
instance DBFractional Float
instance DBFractional Double
instance DBFractional Scientific


-- | The class of database types that support the @/@ operator.
type DBFloating :: Type -> Constraint
class DBFractional a => DBFloating a
instance DBFloating Float
instance DBFloating Double
