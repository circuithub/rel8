{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.DBType
  ( HDBType(..)
  )
where

-- base
import Data.Kind ( Type )
import Data.Proxy ( Proxy( Proxy ) )
import Prelude

-- rel8
import Rel8.Kind.Blueprint ( FromDBType )
import Rel8.Kind.Labels ( SLabels( SLabel ) )
import Rel8.Kind.Necessity ( Necessity( Required ), SNecessity( SRequired ) )
import Rel8.Kind.Nullability
  ( Nullability
  , KnownNullability, nullabilitySing
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec( SSpec ) )
import Rel8.Type ( DBType, blueprintForDBType )


type HDBType :: Nullability -> Type -> HKTable
data HDBType nullability a context where
  HDBType ::
    { unHDBType :: context ('Spec '[""] 'Required nullability (FromDBType a))
    } -> HDBType nullability a (H context)


type HDBTypeField :: Nullability -> Type -> Context
data HDBTypeField nullability a spec where
  HDBTypeField :: HDBTypeField nullability a ('Spec '[""] 'Required nullability (FromDBType a))


instance (KnownNullability nullability, DBType a) => HTable (HDBType nullability a) where
  type HConstrainTable (HDBType nullability a) c =
    c ('Spec '[""] 'Required nullability (FromDBType a))
  type HField (HDBType nullability a) = HDBTypeField nullability a

  hfield (HDBType a) HDBTypeField = a
  htabulate f = HDBType $ f HDBTypeField
  htraverse f (HDBType a) = HDBType <$> f a
  hdicts = HDBType Dict
  hspecs = HDBType (SSpec (SLabel Proxy) SRequired nullabilitySing (blueprintForDBType @a))
