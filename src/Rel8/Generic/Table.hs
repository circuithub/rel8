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
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns, ggtable
  , GGToExprs, ggfromResult, ggtoResult
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
  ( GTableADT, GColumnsADT, gfromColumnsADT, gtoColumnsADT, gtableADT
  , GToExprsADT, gtoResultADT, gfromResultADT
  )
import Rel8.Generic.Table.Record
  ( GTable, GColumns, GContext, gfromColumns, gtoColumns, gtable
  , GToExprs, gtoResult, gfromResult
  )
import Rel8.Kind.Algebra
  ( Algebra( Product, Sum )
  , SAlgebra( SProduct, SSum )
  , KnownAlgebra, algebraSing
  )
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullify )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec )
import Rel8.Schema.Result ( Result )


data GGTable
  :: Algebra
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HContext
  -> (Type -> Type)
  -> Exp Constraint


type instance Eval (GGTable 'Product _Table _Columns context rep) =
  GTable _Table _Columns context rep


type instance Eval (GGTable 'Sum _Table _Columns context rep) =
  GTableADT _Table _Columns context rep


data GGToExprs
  :: Algebra
  -> (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type)
  -> (Type -> Type)
  -> Exp Constraint


type instance Eval (GGToExprs 'Product _ToExprs _Columns exprs rep) =
  GToExprs _ToExprs _Columns exprs rep


type instance Eval (GGToExprs 'Sum _ToExprs _Columns exprs rep) =
  GToExprsADT _ToExprs _Columns exprs rep


data GGColumns
  :: Algebra
  -> (Type -> Exp K.HTable)
  -> (Type -> Type)
  -> Exp K.HTable


type instance Eval (GGColumns 'Product _Columns rep) = GColumns _Columns rep


type instance Eval (GGColumns 'Sum _Columns rep) = GColumnsADT _Columns rep


data GGContext
  :: Algebra
  -> (Type -> Exp K.Context)
  -> (Type -> Type)
  -> Exp K.Context


type instance Eval (GGContext 'Product _Context rep) = GContext _Context rep


type instance Eval (GGContext 'Sum _Context _rep) = Result


type GAlgebra :: (Type -> Type) -> Algebra
type family GAlgebra rep where
  GAlgebra (M1 _ _ rep) = GAlgebra rep
  GAlgebra V1 = 'Sum
  GAlgebra (_ :+: _) = 'Sum
  GAlgebra U1 = 'Sum
  GAlgebra (_ :*: _) = 'Product
  GAlgebra (K1 _ _) = 'Product


ggfromColumns :: forall algebra _Table _Columns rep context x.
  ( KnownAlgebra algebra
  , Eval (GGTable algebra _Table _Columns context rep)
  )
  => (forall spec. algebra ~ 'Sum => context spec -> Col Result spec)
  -> (forall spec. algebra ~ 'Sum => Col Result spec -> context spec)
  -> (forall a. Eval (_Table a) => Eval (_Columns a) context -> a)
  -> Eval (GGColumns algebra _Columns rep) context
  -> rep x
ggfromColumns = case algebraSing @algebra of
  SProduct -> \_ _ -> gfromColumns @_Table @_Columns
  SSum -> gfromColumnsADT @_Table @_Columns


ggtoColumns :: forall algebra _Table _Columns rep context x.
  ( KnownAlgebra algebra
  , Eval (GGTable algebra _Table _Columns context rep)
  )
  => (forall spec. algebra ~ 'Sum => context spec -> Col Result spec)
  -> (forall spec. algebra ~ 'Sum => Col Result spec -> context spec)
  -> (forall a. Eval (_Table a) => a -> Eval (_Columns a) context)
  -> rep x
  -> Eval (GGColumns algebra _Columns rep) context
ggtoColumns = case algebraSing @algebra of
  SProduct -> \_ _ -> gtoColumns @_Table @_Columns
  SSum -> gtoColumnsADT @_Table @_Columns


ggtable :: forall algebra _Table _Columns rep context.
  ( KnownAlgebra algebra
  , Eval (GGTable algebra _Table _Columns context rep)
  )
  => (forall a proxy. Eval (_Table a) => proxy a -> Eval (_Columns a) context)
  -> (forall a labels. ()
      => SSpec ('Spec labels a)
      -> context ('Spec labels a)
      -> context ('Spec labels (Nullify a)))
  -> Eval (GGColumns algebra _Columns rep) context
ggtable = case algebraSing @algebra of
  SProduct -> \table _ -> gtable @_Table @_Columns @_ @rep table
  SSum -> gtableADT @_Table @_Columns @_ @rep


ggfromResult :: forall algebra _ToExprs _Columns exprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGToExprs algebra _ToExprs _Columns exprs rep)
  )
  => (forall expr a proxy. Eval (_ToExprs expr a)
      => proxy expr
      -> Eval (_Columns expr) (Col Result)
      -> a)
  -> Eval (GGColumns algebra _Columns exprs) (Col Result)
  -> rep x
ggfromResult = case algebraSing @algebra of
  SProduct -> gfromResult @_ToExprs @_Columns @exprs
  SSum -> gfromResultADT @_ToExprs @_Columns @exprs


ggtoResult :: forall algebra _ToExprs _Columns exprs rep x.
  ( KnownAlgebra algebra
  , Eval (GGToExprs algebra _ToExprs _Columns exprs rep)
  )
  => (forall expr a proxy. Eval (_ToExprs expr a)
      => proxy expr
      -> a
      -> Eval (_Columns expr) (Col Result))
  -> rep x
  -> Eval (GGColumns algebra _Columns exprs) (Col Result)
ggtoResult = case algebraSing @algebra of
  SProduct -> gtoResult @_ToExprs @_Columns @exprs
  SSum -> gtoResultADT @_ToExprs @_Columns @exprs
