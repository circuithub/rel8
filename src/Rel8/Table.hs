{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table (Columns, Context)
  , toColumns, fromColumns
  , Congruent
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context
  ( Col, Col'(..), Insertion(..), Name, IsSpecialContext
  )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hfield, hspecs, htabulate, htabulateA )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( SSpec( SSpec ), KnownSpec )
import Rel8.Type ( DBType )


type Table :: K.Context -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  type Columns a :: K.HTable
  type Context a :: K.Context

  toColumns :: a -> Columns a (Col (Context a))
  fromColumns :: Columns a (Col (Context a)) -> a


-- | Any 'HTable' is also a 'Table'.
instance (HTable t, x ~ IsSpecialContext context) =>
  Table context (t (Col' x context))
 where
  type Columns (t (Col' x context)) = t
  type Context (t (Col' x context)) = context

  toColumns = id
  fromColumns = id


-- | Any context is trivially a table.
instance (KnownSpec spec, x ~ IsSpecialContext context) =>
  Table context (Col' x context spec)
 where
  type Columns (Col' x context spec) = HIdentity spec
  type Context (Col' x context spec) = context

  toColumns = HIdentity
  fromColumns = unHIdentity


instance Table Expr a => Table Aggregate (Aggregate a) where
  type Columns (Aggregate a) = Columns a
  type Context (Aggregate a) = Aggregate

  toColumns a = htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> Aggregation $ unDB . (`hfield` field) . toColumns <$> a
  fromColumns as = fmap fromColumns $ htabulateA $ \field ->
    case hfield as field of
      Aggregation a -> DB <$> a


instance Sql DBType a => Table Expr (Expr a) where
  type Columns (Expr a) = HType a
  type Context (Expr a) = Expr

  toColumns a = HType (DB a)
  fromColumns (HType (DB a)) = a


instance Sql DBType a => Table Identity (Identity a) where
  type Columns (Identity a) = HType a
  type Context (Identity a) = Identity

  toColumns (Identity a) = HType (Result a)
  fromColumns (HType (Result a)) = Identity a


instance Sql DBType a => Table Insertion (Insertion a) where
  type Columns (Insertion a) = HType a
  type Context (Insertion a) = Insertion

  toColumns (Insertion a) = HType (RequiredInsert a)
  fromColumns (HType (RequiredInsert a)) = Insertion a


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HType a
  type Context (Name a) = Name

  toColumns a = HType (Col a)
  fromColumns (HType (Col a)) = a


instance
  ( Table context a, Table context b
  , Labelable context
  ) =>
  Table context (a, b)
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
  ( Table context a, Table context b, Table context c
  , Labelable context
  ) => Table context (a, b, c)
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
  ( Table context a, Table context b, Table context c, Table context d
  , Labelable context
  ) => Table context (a, b, c, d)
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
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e
  , Labelable context
  ) => Table context (a, b, c, d, e)
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


instance Table Expr Opaque where
  type Columns Opaque = HType Opaque
  type Context Opaque = Expr

  fromColumns = error "opaque"
  toColumns = error "opaque"


instance Table context (Opaque1 context a) where
  type Columns (Opaque1 context a) = HType Opaque
  type Context (Opaque1 context a) = context

  fromColumns = error "opaque"
  toColumns = error "opaque"


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b
