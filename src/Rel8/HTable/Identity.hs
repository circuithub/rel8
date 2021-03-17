{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Rel8.HTable.Identity ( HIdentity(..) ) where

-- rel8
import Rel8.Context ( Context )
import Rel8.DBType ( DBType, typeInformation )
import Rel8.HTable ( HTable( HField, htabulate, htraverse, hfield, hdbtype ) )


-- | A single-column higher-kinded table. This is primarily useful for
-- facilitating generic-deriving of higher kinded tables.
data HIdentity a context where
  HIdentity :: { unHIdentity :: f a } -> HIdentity a (Context f)


data HIdentityField x y where
  HIdentityField :: HIdentityField x x


instance DBType a => HTable (HIdentity a) where
  type HField (HIdentity a) = HIdentityField a

  hfield (HIdentity a) HIdentityField = a
  htabulate f = HIdentity $ f HIdentityField
  hdbtype = HIdentity typeInformation

  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> HIdentity a (Context f) -> m (HIdentity a (Context g))
  htraverse f (HIdentity a) = HIdentity <$> f (a :: f a)
