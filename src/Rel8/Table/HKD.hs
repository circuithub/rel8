{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.HKD
  ( HKD( HKD ), HKDT(..)
  , fromHKD, toHKD
  , GHKD
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Type.Equality ( (:~:)( Refl ) )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude

-- rel8
import Rel8.Column ( Column )
import Rel8.Expr ( Expr )
import Rel8.FCF ( Eval, Exp )
import Rel8.Kind.Algebra ( KnownAlgebra )
import qualified Rel8.Kind.Algebra as K
import Rel8.Generic.Map ( GMap, GMappable, gmap, gunmap )
import Rel8.Generic.Record ( GRecord, GRecordable, grecord, gunrecord )
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GRep, GColumns, GContext, gfromColumns, gtoColumns
  , GColumnsADT, gfromColumnsADT, gtoColumnsADT
  , greify, gunreify
  , TUnreifyContext
  )
import Rel8.Generic.Table
  ( GGTable, GGColumns, GGContext, ggfromColumns, ggtoColumns
  , GAlgebra
  )
import qualified Rel8.Generic.Table.ADT as G
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Reify ( Col( Reify ), Reify, hreify, hunreify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns
  , fromColumns, toColumns, reify, unreify
  , TTable, TColumns, TUnreify
  )
import Rel8.Table.Serialize ( ToExprs, fromResult, toResult )


data TColumn :: K.Context -> Type -> Exp Type
type instance Eval (TColumn f a) = Column f a


type GColumnsHKD :: Type -> K.HTable
type GColumnsHKD a =
  Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify Result)) (Rep a))))


type HKD :: Type -> K.Rel8able
newtype HKD a f = HKD (GColumnsHKD a (Col f))


instance GHKD a => Rel8able (HKD a) where
  type Algebra (HKD a) = 'K.Product

  type GRep (HKD a) f = GRecord (GMap (TColumn f) (Rep a))

  type GColumns (HKD a) = GColumnsHKD a
  type GContext (HKD a) context = context

  gfromColumns = HKD
  gtoColumns (HKD a) = a

  gfromColumnsADT =
    HKD .
    ggtoColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      toColumns .
    G.gfromColumnsADT
      @(TTable (Reify Result))
      @TColumns
      @(Col (Reify Result))
      @(GRep (HKD a) (Reify Result))
      (\(Reify a) -> a)
      Reify
      fromColumns

  gtoColumnsADT =
    G.gtoColumnsADT
      @(TTable (Reify Result))
      @TColumns
      @(Col (Reify Result))
      @(GRep (HKD a) (Reify Result))
      (\(Reify a) -> a)
      Reify
      toColumns .
    ggfromColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify Result))
      @TColumns
      (\(Reify a) -> a)
      Reify
      fromColumns .
      (\(HKD a) -> a)

  greify (HKD a) = HKD (hreify a)
  gunreify (HKD a) = HKD (hunreify a)


instance
  ( KnownAlgebra (GAlgebra (Rep a))
  , HTable (GColumnsHKD a)
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify f)) TColumns (Col (Reify f)) (GRecord (GMap (TColumn (Reify f)) (Rep a))))
  , Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify f)) (Rep a)))) ~ GColumnsHKD a
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify f)) (Rep a)))) ~ f
  , GRecordable (GMap (TColumn (Reify f)) (Rep a))
  , GMappable (TTable (Reify f)) (GMap (TColumn (Reify f)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify f)) (Rep a)) ~ GMap (TColumn f) (Rep a)
  )
  => Generic (HKD a f)
 where
  type Rep (HKD a f) = GMap (TColumn f) (Rep a)

  from =
    gmap @(TTable (Reify f)) (Proxy @TUnreify) (unreify Refl) .
    gunrecord @(GMap (TColumn (Reify f)) (Rep a)) .
    ggfromColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify f))
      @TColumns
      (\(Reify a) -> a)
      Reify
      fromColumns .
    hreify .
    (\(HKD a) -> a)

  to =
    HKD .
    hunreify .
    ggtoColumns
      @(GAlgebra (Rep a))
      @(TTable (Reify f))
      @TColumns
      (\(Reify a) -> a)
      Reify
      toColumns .
    grecord @(GMap (TColumn (Reify f)) (Rep a)) .
    gunmap @(TTable (Reify f)) (Proxy @TUnreify) (reify Refl)


type HKDT :: Type -> Type
newtype HKDT a = HKDT
  { unHKDT :: a
  }


instance
  ( Table Expr (HKD a Expr)
  , Columns (HKD a Expr) ~ GColumns (HKD a)
  , GHKD a
  , x ~ HKD a Expr
  )
  => ToExprs x (HKDT a)
 where
  toResult = (\(HKD a) -> a) . toHKD . (\(HKDT a) -> a)
  fromResult = HKDT . fromHKD . HKD


fromHKD :: GHKD a => HKD a Result -> a
fromHKD = to . gunmap @Top (Proxy @(TColumn Result)) id . from


toHKD :: GHKD a => a -> HKD a Result
toHKD = to . gmap @Top (Proxy @(TColumn Result)) id . from


data Top :: Type -> Exp Constraint
type instance Eval (Top _) = ()


class
  ( Generic a
  , HTable (GColumns (HKD a))
  , HTable (GColumnsADT (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a))))
  , Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ GColumnsHKD a
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ Result
  , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a)))
  , GRecordable (GMap (TColumn (Reify Result)) (Rep a))
  , GMappable Top (Rep a)
  , GMappable (TTable (Reify Result)) (GMap (TColumn (Reify Result)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify Result)) (Rep a)) ~ GMap (TColumn Result) (Rep a)
  )
  => GHKD a
instance
  ( Generic a
  , HTable (GColumns (HKD a))
  , HTable (GColumnsADT (HKD a))
  , KnownAlgebra (GAlgebra (Rep a))
  , Eval (GGTable (GAlgebra (Rep a)) (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a))))
  , Eval (GGColumns (GAlgebra (Rep a)) TColumns (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ GColumnsHKD a
  , Eval (GGContext (GAlgebra (Rep a)) TUnreifyContext (GRecord (GMap (TColumn (Reify Result)) (Rep a)))) ~ Result
  , G.GTableADT (TTable (Reify Result)) TColumns (Col (Reify Result)) (GRecord (GMap (TColumn (Reify Result)) (Rep a)))
  , GRecordable (GMap (TColumn (Reify Result)) (Rep a))
  , GMappable Top (Rep a)
  , GMappable (TTable (Reify Result)) (GMap (TColumn (Reify Result)) (Rep a))
  , GMap TUnreify (GMap (TColumn (Reify Result)) (Rep a)) ~ GMap (TColumn Result) (Rep a)
  )
  => GHKD a
