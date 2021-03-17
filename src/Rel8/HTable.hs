{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}

module Rel8.HTable ( HTable(..), hmap, hzipWith ) where

import Data.Kind ( Constraint, Type )
import Rel8.Context ( KContext, Context )
import Rel8.Type ( TypeInformation )

type HTable :: (KContext -> Type) -> Constraint
class HTable t where
  type HField t = (field :: Type -> Type) | field -> t
  type HConstrainTable t (c :: Type -> Constraint) :: Constraint

  hfield :: t (Context f) -> HField t x -> f x
  htabulate :: forall f. (forall x. HField t x -> f x) -> t (Context f)
  htraverse :: forall f g m. Applicative m => (forall x. f x -> m (g x)) -> t (Context f) -> m (t (Context g))
  hdbtype :: t (Context TypeInformation)


hmap :: HTable t => (forall x. f x -> g x) -> t (Context f) -> t (Context g)
hmap f t = htabulate $ f <$> hfield t


hzipWith :: HTable t => (forall x. f x -> g x -> h x) -> t (Context f) -> t (Context g) -> t (Context h)
hzipWith f t u = htabulate $ f <$> hfield t <*> hfield u
