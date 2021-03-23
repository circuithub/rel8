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
import Rel8.Aggregate ( Aggregate, Col( Aggregation ) )
import Rel8.Expr ( Expr, Col( DB ), unDB )
import Rel8.Opaque ( Opaque, Opaque1 )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hfield, hspecs, htabulate, htabulateA )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( SSpec( SSpec ), KnownSpec, Interpretation( Col ), Col( Result ) )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Type ( DBType )


type Table :: (Type -> Type) -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  type Columns a :: HKTable
  type Context a :: Type -> Type

  toColumns :: a -> Columns a (Col context)
  fromColumns :: Columns a (Col context) -> a


-- | Any 'HTable' is also a 'Table'.
instance (HTable t, f ~ g) => Table f (t (Col g)) where
  type Columns (t (Col g)) = t
  type Context (t (Col g)) = g

  toColumns = id
  fromColumns = id


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
