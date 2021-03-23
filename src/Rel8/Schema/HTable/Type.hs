{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Type
  ( HType(..)
  )
where

-- base
import Data.Kind ( Type )
import Data.Proxy ( Proxy( Proxy ) )
import Prelude

-- rel8
import Rel8.Kind.Labels ( SLabels( SLabel ) )
import Rel8.Kind.Necessity ( Necessity( Required ), SNecessity( SRequired ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Nullability ( Unnullify, Sql, nullabilization )
import Rel8.Schema.Spec ( Context, Spec( Spec ), SSpec(..) )
import Rel8.Type ( DBType, typeInformation )


type HType :: Type -> HKTable
data HType a context where
  HType ::
    { unHType :: context ('Spec '[""] 'Required (Unnullify a) a)
    } -> HType a (H context)


type HTypeField :: Type -> Context
data HTypeField a spec where
  HTypeField :: HTypeField a ('Spec '[""] 'Required (Unnullify a) a)


instance Sql DBType a => HTable (HType a) where
  type HConstrainTable (HType a) c = c ('Spec '[""] 'Required (Unnullify a) a)
  type HField (HType a) = HTypeField a

  hfield (HType a) HTypeField = a
  htabulate f = HType $ f HTypeField
  htraverse f (HType a) = HType <$> f a
  hdicts = HType Dict
  hspecs = HType SSpec
    { labels = SLabel Proxy
    , necessity = SRequired
    , info = typeInformation
    , nullability = nullabilization
    }
