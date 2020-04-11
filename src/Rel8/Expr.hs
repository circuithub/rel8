{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

module Rel8.Expr where

import Control.Applicative ( Const(..) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Data.Functor.Sum ( Sum(..) )
import Data.Indexed.Functor.Identity ( HIdentity )
import Data.Indexed.Functor.Product ( HProduct )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Singletons ( TyCon1 )
import Data.Singletons.Prelude ( Fmap )
import Data.Singletons.Prelude.Foldable (Asum)
import Data.Singletons.Prelude.Maybe ( FromJust )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Data.Type.Demote ( Demote(..) )
import Data.Type.Equality ( (:~:)(..), TestEquality(..) )
import GHC.Records.Compat ( HasField(..) )
import GHC.TypeLits ( Symbol )
import Rel8.Primitive
import qualified Rel8.SQL as SQL
import Rel8.Table ( Table(..) )


-- | Typed expressions.
newtype Expr a =
  Expr (Pattern a Expr)


-- Try and find the path to a given named field.
type family FindName k (name :: Symbol) f :: Maybe k where
  FindName (Product (Const ()) f r) name (Compose (Tagged (t :: *)) g) =
    Fmap (TyCon1 ('Pair ('Const '()))) (FindName (f r) name g)

  FindName (Product (Const ()) ((:~:) x) x) name (Compose (Tagged name) (HIdentity r)) =
    'Just ('Pair ('Const '()) 'Refl)

  FindName (Sum (Product (Const ()) ((:~:) r)) _ r) name (HProduct (Compose (Tagged name) (HIdentity r)) _) =
    'Just ('InL ('Pair ('Const '()) 'Refl))

  FindName (Sum l r x) name (HProduct f g) =
    Asum
      '[ Fmap (TyCon1 'InL ) (FindName (l x) name f)
       , Fmap (TyCon1 'InR) (FindName (r x) name g)
       ]

  FindName _ _ _ =
    'Nothing



instance (Demote (FromJust (FindName (HRep (Pattern a) r) name (Pattern a))), HasField name a r, HRepresentable (Pattern a), TestEquality (HRep (Pattern a))) => HasField (name :: Symbol) (Expr a) (Expr r) where
  hasField (Expr x) = (setter, getter) where
    i :: HRep (Pattern a) r
    i = demote @_ @(FromJust (FindName (HRep (Pattern a) r) name (Pattern a)))

    setter :: Expr r -> Expr a
    setter y = Expr $ htabulate \j ->
      case testEquality i j of
        Just Refl -> y
        Nothing -> hindex x j

    getter = hindex @(Pattern a) x i


instance PrimLike Expr where
  data Primitive a Expr = PrimExpr SQL.Expr


fst_ :: Expr (a, b) -> Expr a
fst_ (Expr x) = hindex x $ Pair (Const ()) $ InL Refl


snd_ :: Expr (a, b) -> Expr b
snd_ (Expr x) = hindex x $ Pair (Const ()) $ InR Refl


isNothing :: Expr (Maybe a) -> Expr Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
maybe_ = undefined


class DBType a where
  lit :: a -> Expr a


instance DBType Bool where
  lit = undefined
