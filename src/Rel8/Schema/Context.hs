{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context
  ( Interpretation, Col(..)
  , Insertion(..)
  , Name(..)
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


type Interpretation :: K.Context -> Constraint
class Interpretation context where
  data Col context :: K.HContext


instance Interpretation Aggregate where
  data Col Aggregate _spec where
    Aggregation :: ()
      => Aggregate (Expr a)
      -> Col Aggregate ('Spec labels necessity db a)


instance Interpretation Expr where
  data Col Expr _spec where
    DB :: ()
      => { unDB :: Expr a }
      -> Col Expr ('Spec labels necessity db a)


instance Interpretation Identity where
  data Col Identity _spec where
    Result :: a -> Col Identity ('Spec labels necessity db a)


instance Interpretation Insertion where
  data Col Insertion _spec where
    RequiredInsert :: Expr a -> Col Insertion ('Spec labels 'Required db a)
    OptionalInsert :: Maybe (Expr a) -> Col Insertion ('Spec labels 'Optional db a)


instance Interpretation Name where
  data Col Name _spec where
    NameCol :: String -> Col Name ('Spec labels necessity db a)


type Insertion :: K.Context
newtype Insertion a = Insertion (Expr a)


-- | A @Name@ is the name of a column, as it would be defined in a table's
-- schema definition. You can construct names by using the @OverloadedStrings@
-- extension and writing string literals. This is typically done when providing
-- a 'TableSchema' value.
type Name :: K.Context
newtype Name a = Name String
  deriving stock Show
  deriving newtype (IsString, Monoid, Semigroup)
