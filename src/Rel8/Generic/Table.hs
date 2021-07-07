{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Table
  ( GGSerialize, GGColumns, ggfromResult, ggtoResult
  , GAlgebra
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( (:+:), (:*:), K1, M1, U1, V1 )
import Prelude ()

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Table.ADT
  ( GSerializeADT, GColumnsADT, gtoResultADT, gfromResultADT
  )
import Rel8.Generic.Table.Record ( GSerialize, GColumns, gtoResult, gfromResult )
import Rel8.Kind.Algebra
  ( Algebra( Product, Sum )
  , SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )


data GGSerialize
  :: Algebra
  -> (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type)
  -> (Type -> Type)
  -> Exp Constraint


type instance Eval (GGSerialize 'Product _Serialize _Columns exprs rep) =
  GSerialize _Serialize _Columns exprs rep


type instance Eval (GGSerialize 'Sum _Serialize _Columns exprs rep) =
  GSerializeADT _Serialize _Columns exprs rep


data GGColumns
  :: Algebra
  -> (Type -> Exp K.HTable)
  -> (Type -> Type)
  -> Exp K.HTable


type instance Eval (GGColumns 'Product _Columns rep) = GColumns _Columns rep


type instance Eval (GGColumns 'Sum _Columns rep) = GColumnsADT _Columns rep


type GAlgebra :: (Type -> Type) -> Algebra
type family GAlgebra rep where
  GAlgebra (M1 _ _ rep) = GAlgebra rep
  GAlgebra V1 = 'Sum
  GAlgebra (_ :+: _) = 'Sum
  GAlgebra U1 = 'Sum
  GAlgebra (_ :*: _) = 'Product
  GAlgebra (K1 _ _) = 'Product


ggfromResult :: forall algebra _Serialize _Columns exprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGSerialize algebra _Serialize _Columns exprs rep)
  )
  => (forall expr a proxy. Eval (_Serialize expr a)
      => proxy expr -> Eval (_Columns expr) Result -> a)
  -> Eval (GGColumns algebra _Columns exprs) Result
  -> rep x
ggfromResult f x = case algebraSing @algebra of
  SProduct -> gfromResult @_Serialize @_Columns @exprs @rep f x
  SSum -> gfromResultADT @_Serialize @_Columns @exprs @rep f x


ggtoResult :: forall algebra _Serialize _Columns exprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGSerialize algebra _Serialize _Columns exprs rep)
  )
  => (forall expr a proxy. Eval (_Serialize expr a)
      => proxy expr -> a -> Eval (_Columns expr) Result)
  -> rep x
  -> Eval (GGColumns algebra _Columns exprs) Result
ggtoResult f x = case algebraSing @algebra of
  SProduct -> gtoResult @_Serialize @_Columns @exprs @rep f x
  SSum -> gtoResultADT @_Serialize @_Columns @exprs @rep f x
