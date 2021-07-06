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
  ( GGTable, GGColumns, ggfromResult, ggtoResult
  , GAlgebra
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( (:+:), (:*:), K1, M1, U1, V1 )
import Prelude ()

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap )
import Rel8.Generic.Table.ADT
  ( GTableADT, GColumnsADT, gtoResultADT, gfromResultADT
  )
import Rel8.Generic.Table.Record ( GTable, GColumns, gtoResult, gfromResult )
import Rel8.Kind.Algebra
  ( Algebra( Product, Sum )
  , SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )


data GGTable
  :: Algebra
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> (Type -> Type)
  -> Exp Constraint


type instance Eval (GGTable 'Product _Table _Columns _FromExprs rep) =
  GTable _Table _Columns _FromExprs rep


type instance Eval (GGTable 'Sum _Table _Columns _FromExprs rep) =
  GTableADT _Table _Columns _FromExprs rep


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


ggfromResult :: forall algebra _Table _Columns _FromExprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGTable algebra _Table _Columns _FromExprs rep)
  )
  => (forall a proxy. Eval (_Table a)
      => proxy a -> Eval (_Columns a) (Col Result) -> Eval (_FromExprs a))
  -> Eval (GGColumns algebra _Columns rep) (Col Result)
  -> GMap _FromExprs rep x
ggfromResult f x = case algebraSing @algebra of
  SProduct -> gfromResult @_Table @_Columns @_FromExprs @rep f x
  SSum -> gfromResultADT @_Table @_Columns @_FromExprs @rep f x


ggtoResult :: forall algebra _Table _Columns _FromExprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGTable algebra _Table _Columns _FromExprs rep)
  )
  => (forall a proxy. Eval (_Table a)
      => proxy a -> Eval (_FromExprs a) -> Eval (_Columns a) (Col Result))
  -> GMap _FromExprs rep x
  -> Eval (GGColumns algebra _Columns rep) (Col Result)
ggtoResult f x = case algebraSing @algebra of
  SProduct -> gtoResult @_Table @_Columns @_FromExprs @rep f x
  SSum -> gtoResultADT @_Table @_Columns @_FromExprs @rep f x
