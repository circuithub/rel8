{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Rel8.HTable.HPair ( HPair(..) ) where

-- base
import Data.Kind ( Type )
import GHC.Generics ( Generic )

-- rel8
import Rel8.Context ( Meta )
import Rel8.HTable ( HTable( HField, hfield, htabulate, htraverse, hdbtype ), HAllColumns )

-- semigroupoids
import Data.Functor.Apply ((<.>))


-- | Pair two higher-kinded tables. This is primarily used to facilitate
-- generic deriving of higher-kinded tables with more than 1 field (it deals
-- with the @:*:@ case).
data HPair x y (f :: Meta -> Type) = HPair { hfst :: x f, hsnd :: y f }
  deriving stock (Generic)


-- | A HField type for indexing into HPair.
data HPairField x y a where
  HPairFst :: HField x a -> HPairField x y a
  HPairSnd :: HField y a -> HPairField x y a


instance (HTable x, HTable y) => HTable (HPair x y) where
  type HField (HPair x y) = HPairField x y
  type HAllColumns (HPair x y) c = (HAllColumns x c, HAllColumns y c)

  hfield (HPair l r) = \case
    HPairFst i -> hfield l i
    HPairSnd i -> hfield r i

  htabulate f = HPair (htabulate (f . HPairFst)) (htabulate (f . HPairSnd))

  htraverse f (HPair x y) = HPair <$> htraverse f x <.> htraverse f y

  hdbtype = HPair hdbtype hdbtype
