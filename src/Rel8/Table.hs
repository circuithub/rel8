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
import {-# SOURCE #-} Rel8.Expr ( Expr )
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( KnownSpec )
import Rel8.Type ( DBType )


-- | @Table@s are one of the foundational elements of Rel8, and describe data
-- types that have a finite number of columns. Each of these columns contains
-- data under a shared context, and contexts describe how to interpret the
-- metadata about a column to a particular Haskell type. In Rel8, we have
-- contexts for expressions (the 'Rel8.Expr' context), aggregations (the
-- 'Rel8.Aggregate' context), insert values (the 'Rel8.Insert' contex), among
-- others.
--
-- In typical usage of Rel8 you don't need to derive instances of 'Table'
-- yourself, as anything that's an instance of 'Rel8.Rel8able' is always a
-- 'Table'.
type Table :: K.Context -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  -- | The 'HTable' functor that describes the schema of this table.
  type Columns a :: K.HTable

  -- | The common context that all columns use as an interpretation.
  type Context a :: K.Context

  toColumns :: a -> Columns a (Col (Context a))
  fromColumns :: Columns a (Col (Context a)) -> a


-- | Any 'HTable' is also a 'Table'.
instance HTable t => Table context (t (Col context)) where
  type Columns (t (Col context)) = t
  type Context (t (Col context)) = context

  toColumns = id
  fromColumns = id


-- | Any context is trivially a table.
instance KnownSpec spec => Table context (Col context spec) where
  type Columns (Col context spec) = HIdentity spec
  type Context (Col context spec) = context

  toColumns = HIdentity
  fromColumns = unHIdentity


instance Sql DBType a => Table Identity (Identity a) where
  type Columns (Identity a) = HType a
  type Context (Identity a) = Identity

  toColumns (Identity a) = HType (Result a)
  fromColumns (HType (Result a)) = Identity a


{-}
instance Sql DBType a => Table Insertion (Insertion a) where
  type Columns (Insertion a) = HType a
  type Context (Insertion a) = Insertion

  toColumns (Insertion a) = HType (RequiredInsert a)
  fromColumns (HType (RequiredInsert a)) = Insertion a
-}


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


instance Table context (Opaque context a) where
  type Columns (Opaque context a) = HType Opaque
  type Context (Opaque context a) = context

  fromColumns = error "opaque"
  toColumns = error "opaque"


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b
