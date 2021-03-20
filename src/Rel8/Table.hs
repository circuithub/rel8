{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table (Columns, Context)
  , toColumns, fromColumns
  , Congruent
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Blueprint
  ( ToDBType
  , FromType
  , blueprintRoundtripsViaDBType
  , blueprintRoundtripsViaType
  , dbTypeRoundtripsViaBlueprint
  , simplifyDBTypeBlueprint
  )
import Rel8.Kind.Nullability ( KnownNullability )
import Rel8.Schema.Context
  ( Aggregation( Aggregation )
  , DB( DB ), unDB
  , Result( Result )
  )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hfield, hspecs, htabulate, htabulateA )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.HTable.DBType ( HDBType( HDBType ) )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.Spec ( SSpec( SSpec ), KnownSpec )
import qualified Rel8.Schema.Spec as Kind ( Context )
import Rel8.Schema.Value ( Value )
import Rel8.Type ( DBType, blueprintForDBType )


type Table :: Kind.Context -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  type Columns a :: HKTable
  type Context a :: Kind.Context

  toColumns :: a -> Columns a (H (Context a))
  fromColumns :: Columns a (H (Context a)) -> a


-- | Any 'HTable' is also a 'Table'.
instance HTable t => Table context (t (H context)) where
  type Columns (t (H context)) = t
  type Context (t (H context)) = context

  toColumns = id
  fromColumns = id


-- | Any context is trivially a table.
instance KnownSpec spec => Table context (context spec) where
  type Columns (context spec) = HIdentity spec
  type Context (context spec) = context

  toColumns = HIdentity
  fromColumns = unHIdentity


instance Table DB a => Table Aggregation (Aggregate a) where
  type Columns (Aggregate a) = Columns a
  type Context (Aggregate a) = Aggregation

  toColumns a = htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> Aggregation $ unDB . (`hfield` field) . toColumns <$> a
  fromColumns as = fmap fromColumns $ htabulateA $ \field ->
    case hfield as field of
      Aggregation a -> DB <$> a


instance (KnownNullability nullability, DBType a) =>
  Table DB (Expr nullability a)
 where
  type Columns (Expr nullability a) = HDBType nullability a
  type Context (Expr nullability a) = DB

  toColumns a = case blueprintForDBType @a of
    blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
      Refl -> HDBType (DB a)
  fromColumns (HDBType (DB a)) = case blueprintForDBType @a of
    blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
      Refl -> a


instance (KnownNullability nullability, DBType (ToDBType (FromType a))) =>
  Table Result (Value nullability a)
 where
  type Columns (Value nullability a) =
    HDBType nullability (ToDBType (FromType a))
  type Context (Value nullability a) = Result

  toColumns a = case blueprintForDBType @(ToDBType (FromType a)) of
    blueprint -> case simplifyDBTypeBlueprint blueprint of
      blueprint' ->
        case dbTypeRoundtripsViaBlueprint @(FromType a) blueprint' of
          Refl -> case blueprintRoundtripsViaType @a blueprint of
            Refl -> HDBType $ Result a
  fromColumns (HDBType (Result a)) =
    case blueprintForDBType @(ToDBType (FromType a)) of
      blueprint -> case simplifyDBTypeBlueprint blueprint of
        blueprint' ->
          case dbTypeRoundtripsViaBlueprint @(FromType a) blueprint' of
            Refl -> case blueprintRoundtripsViaType @a blueprint of
              Refl -> a


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


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b
