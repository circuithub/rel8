{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table (Columns, Context)
  , toColumns, fromColumns
  , Congruent, Compatible
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Blueprint
  ( KnownBlueprint
  , FromDBType, ToDBType
  , FromType, ToType
  )
import Rel8.Kind.Necessity ( Necessity( Required ) )
import Rel8.Kind.Nullability ( KnownNullability )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB )
  , Result( Result )
  )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.Spec ( Spec( Spec ), KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Schema.Value ( Value )
import Rel8.Type ( DBType )


type Table :: Type -> Constraint
class HTable (Columns a) => Table a where
  type Columns a :: HKTable
  type Context a :: Kind.Context

  toColumns :: a -> Columns a (H (Context a))
  fromColumns :: Columns a (H (Context a)) -> a


-- | Any 'HTable' is also a 'Table'.
instance HTable t => Table (t (H context)) where
  type Columns (t (H context)) = t
  type Context (t (H context)) = context

  toColumns = id
  fromColumns = id


-- | Any context is trivially a table.
instance KnownSpec spec => Table (context spec) where
  type Columns (context spec) = HIdentity spec
  type Context (context spec) = context

  toColumns = HIdentity
  fromColumns = unHIdentity


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Table (Aggregate nullability a)
 where
  type Columns (Aggregate nullability a) =
    HIdentity ('Spec '["anonymous"] 'Required nullability (FromDBType a))
  type Context (Aggregate nullability a) = Aggregation

  toColumns a = HIdentity (Aggregation a)
  fromColumns (HIdentity (Aggregation a)) = a


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , DBType a
  ) =>
  Table (Expr nullability a)
 where
  type Columns (Expr nullability a) =
    HIdentity ('Spec '["anonymous"] 'Required nullability (FromDBType a))
  type Context (Expr nullability a) = DB

  toColumns a = HIdentity (DB a)
  fromColumns (HIdentity (DB a)) = a


instance
  ( KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromType a
  , ToType blueprint ~ a
  , DBType (ToDBType blueprint)
  ) =>
  Table (Value nullability a)
 where
  type Columns (Value nullability a) =
    HIdentity ('Spec '["anonymous"] 'Required nullability (FromType a))
  type Context (Value nullability a) = Result

  toColumns a = HIdentity (Result a)
  fromColumns (HIdentity (Result a)) = a


instance
  ( Table a, Table b
  , Compatible a b
  , Labelable (Context a)
  ) => Table (a, b)
 where
  type Columns (a, b) =
    HPair
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
  type Context (a, b) = Context a

  toColumns (a, b) = HPair
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    }
  fromColumns (HPair a b) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    )


instance
  ( Table a, Table b, Table c
  , Compatible a b, Compatible b c
  , Labelable (Context a)
  ) => Table (a, b, c)
 where
  type Columns (a, b, c) =
    HTrio
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
  type Context (a, b, c) = Context a

  toColumns (a, b, c) = HTrio
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    }
  fromColumns (HTrio a b c) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    )


instance
  ( Table a, Table b, Table c, Table d
  , Compatible a b, Compatible b c, Compatible c d
  , Labelable (Context a)
  ) => Table (a, b, c, d)
 where
  type Columns (a, b, c, d) =
    HQuartet
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
      (HLabel "frt" (Columns d))
  type Context (a, b, c, d) = Context a

  toColumns (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    , hfrt = hlabel labeler $ toColumns d
    }
  fromColumns (HQuartet a b c d) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    , fromColumns $ hunlabel unlabeler d
    )


instance
  ( Table a, Table b, Table c, Table d, Table e
  , Compatible a b, Compatible b c, Compatible c d, Compatible d e
  , Labelable (Context a)
  ) => Table (a, b, c, d, e)
 where
  type Columns (a, b, c, d, e) =
    HQuintet
      (HLabel "fst" (Columns a))
      (HLabel "snd" (Columns b))
      (HLabel "trd" (Columns c))
      (HLabel "frt" (Columns d))
      (HLabel "fft" (Columns e))
  type Context (a, b, c, d, e) = Context a

  toColumns (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toColumns a
    , hsnd = hlabel labeler $ toColumns b
    , htrd = hlabel labeler $ toColumns c
    , hfrt = hlabel labeler $ toColumns d
    , hfft = hlabel labeler $ toColumns e
    }
  fromColumns (HQuintet a b c d e) =
    ( fromColumns $ hunlabel unlabeler a
    , fromColumns $ hunlabel unlabeler b
    , fromColumns $ hunlabel unlabeler c
    , fromColumns $ hunlabel unlabeler d
    , fromColumns $ hunlabel unlabeler e
    )


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b


type Compatible :: Type -> Type -> Constraint
class Context a ~ Context b => Compatible a b
instance Context a ~ Context b => Compatible a b
