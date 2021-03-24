{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Context
  ( Col'( .. )
  , Interpretation, Col
  , Insertion(..)
  , Name(..)
  , IsSpecialContext
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Necessity ( Necessity( Optional, Required ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Schema.Structure ( Structure )


type Interpretation' :: Bool -> K.Context -> Constraint
class IsSpecialContext f ~ isSpecial => Interpretation' isSpecial f where
  data Col' isSpecial f :: K.HContext


instance Interpretation' 'True Aggregate where
  data Col' 'True Aggregate _spec where
    Aggregation :: ()
      => Aggregate (Expr a)
      -> Col' 'True Aggregate ('Spec labels necessity db a)


instance Interpretation' 'True Expr where
  data Col' 'True Expr _spec where
    DB :: ()
      => { unDB :: Expr a }
      -> Col' 'True Expr ('Spec labels necessity db a)


instance Interpretation' 'True Identity where
  data Col' 'True Identity _spec where
    Result :: a -> Col' 'True Identity ('Spec labels necessity db a)


instance Interpretation' 'True Insertion where
  data Col' 'True Insertion _spec where
    RequiredInsert :: Expr a -> Col' 'True Insertion ('Spec labels 'Required db a)
    OptionalInsert :: Maybe (Expr a) -> Col' 'True Insertion ('Spec labels 'Optional db a)


instance IsSpecialContext f ~ 'False => Interpretation' 'False f where
  data Col' 'False f _spec where
    Col :: f a -> Col' 'False f ('Spec labels necessity db a)


type Interpretation :: K.Context -> Constraint
class Interpretation' (IsSpecialContext context) context => Interpretation context
instance Interpretation' (IsSpecialContext context) context => Interpretation context


type Col :: K.Context -> K.HContext
type Col context = Col' (IsSpecialContext context) context


type Insertion :: K.Context
newtype Insertion a = Insertion (Expr a)


type Name :: K.Context
newtype Name a = Name String
  deriving stock Show
  deriving newtype (IsString, Monoid, Semigroup)


type IsSpecialContext :: K.Context -> Bool
type family IsSpecialContext context where
  IsSpecialContext Aggregate = 'True
  IsSpecialContext Expr = 'True
  IsSpecialContext Insertion = 'True
  IsSpecialContext Identity = 'True
  IsSpecialContext Structure = 'True
  IsSpecialContext _ = 'False
