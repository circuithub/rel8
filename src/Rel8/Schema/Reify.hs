{-# language AllowAmbiguousTypes #-}
{-# language EmptyCase #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Reify
  ( Reify, Col( Reify ), hreify, hunreify
  , UnwrapReify
  , NotReify, notReify
  )
where

-- base
import Data.Kind ( Constraint )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hfield, htabulate )
import Rel8.Schema.Kind ( Context )


type Reify :: Context -> Context
data Reify context a


instance Interpretation (Reify context) where
  newtype Col (Reify context) spec = Reify (Col context spec)


instance Labelable context => Labelable (Reify context) where
  labeler (Reify a) = Reify (labeler a)
  unlabeler (Reify a) = Reify (unlabeler a)


hreify :: HTable t => t (Col context) -> t (Col (Reify context))
hreify a = htabulate $ \field -> Reify (hfield a field)


hunreify :: HTable t => t (Col (Reify context)) -> t (Col context)
hunreify a = htabulate $ \field -> case hfield a field of
  Reify x -> x


type UnwrapReify :: Context -> Context
type family UnwrapReify context where
  UnwrapReify (Reify context) = context


type IsReify :: Context -> Bool
type family IsReify context where
  IsReify (Reify _) = 'True
  IsReify _ = 'False


type NotReify :: Context -> Constraint
class IsReify context ~ 'False => NotReify context
instance IsReify context ~ 'False => NotReify context


notReify :: forall context ctx a. NotReify context => context :~: Reify ctx -> a
notReify refl = case lemma @context of
  Refl -> case refl of


lemma :: NotReify context => IsReify context :~: 'False
lemma = Refl
