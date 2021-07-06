{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Column
  ( Column
  , TColumn
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name(..) )
import Rel8.Schema.Result ( Result )


-- | This type family is used to specify columns in 'Rel8able's. In @Column f
-- a@, @f@ is the context of the column (which should be left polymorphic in
-- 'Rel8able' definitions), and @a@ is the type of the column.
type Column :: K.HContext -> Type -> Type
type family Column context a where
  Column Aggregate       a = Aggregate a
  Column Expr            a = Expr a
  Column Name            a = Name a
  Column Result          a = a


data TColumn :: K.HContext -> Type -> Exp Type
type instance Eval (TColumn f a) = Column f a
