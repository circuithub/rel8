{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Category.Projection
  ( Projection( Projection )
  , Projectable, projection
  , project
  )
where

-- base
import Control.Category ( Category, (.), id )
import Data.Kind ( Constraint, Type )
import GHC.TypeLits ( ErrorMessage( (:$$:), Text ), TypeError )
import Prelude hiding ( Functor, (.), fmap, fst, id, product, snd )

-- categories
import Control.Categorical.Functor ( Endofunctor, Functor, fmap )
import Control.Categorical.Bifunctor
  ( Bifunctor, bimap
  , PFunctor, first
  , QFunctor, second
  )
import Control.Category.Associative ( Associative, associate, disassociate )
import Control.Category.Braided ( Braided, braid, Symmetric )
import Control.Category.Cartesian ( Cartesian, Product, (&&&), diag, fst, snd )
import Control.Category.Monoidal ( Monoidal, Id, idl, idr, coidl, coidr )

-- rel8
import Rel8.Schema.Field ( Field( F, Field ), fields )
import Rel8.Schema.HTable ( hfield, htabulate )
import Rel8.Schema.HTable.Label ( hlabel, hrelabel, hunlabel )
import qualified Rel8.Schema.HTable.Label as Label ( hproject )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Recontextualize )

-- semigroupoids
import Data.Semigroupoid ( Semigroupoid, o )


-- | The category of 'Projection's. A @'Projection' a b@ differs from a
-- function @a -> b@ in that the @b@ must be solely composed from the columns
-- contained in @a@. 'Projection' has no 'const' or 'pure', so @b@ can never
-- be just be a constant or literal value; it has to be projected from @a@.
--
-- You can use 'projection' to construct 'Projection's, and 'project' to
-- \"run\" them.
type Projection :: Type -> Type -> Type
newtype Projection a b =
  Projection (forall context. Columns a context -> Columns b context)


instance Semigroupoid Projection where
  Projection f `o` Projection g = Projection (f `o` g)


instance Category Projection where
  id = Projection id
  (.) = o


instance Cartesian Projection where
  type Product Projection = (,)

  fst = Projection $ \(HProduct a _) -> hunlabel a
  snd = Projection $ \(HProduct _ b) -> hunlabel b
  diag = Projection $ \a -> HProduct (hlabel a) (hlabel a)
  Projection f &&& Projection g = Projection $ \a ->
    case f a of
      b -> case g a of
        c -> HProduct (hlabel b) (hlabel c)


instance Bifunctor (,) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $ \(HProduct a c) ->
    HProduct (Label.hproject f a) (Label.hproject g c)


instance Associative Projection (,) where
  associate = Projection $ \(HProduct ab c) -> case hunlabel ab of
    HProduct a b -> HProduct a (hlabel (HProduct (hrelabel b) c))
  disassociate = Projection $ \(HProduct a bc) -> case hunlabel bc of
    HProduct b c -> HProduct (hlabel (HProduct a (hrelabel b))) c


instance Braided Projection (,) where
  braid = Projection $ \(HProduct a b) -> HProduct (hrelabel b) (hrelabel a)


instance Symmetric Projection (,)


instance PFunctor (,) Projection Projection where
  first f = bimap f id


instance QFunctor (,) Projection Projection where
  second = bimap id


instance Functor ((,) x) Projection Projection where
  fmap = second


instance Endofunctor ((,) x) Projection


instance Bifunctor ((,,) x) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $ \(HProduct x (HProduct a b)) ->
    HProduct x (HProduct (Label.hproject f a) (Label.hproject g b))


instance Associative Projection ((,,) x) where
  associate = Projection $ \(HProduct x (HProduct x'ab c)) -> case hunlabel x'ab of
    HProduct x' (HProduct a b) ->
      HProduct x (HProduct a (hlabel (HProduct x' (HProduct (hrelabel b) c))))

  disassociate = Projection $ \(HProduct x (HProduct a x'bc)) -> case hunlabel x'bc of
    HProduct x' (HProduct b c) ->
      HProduct x (HProduct (hlabel (HProduct x' (HProduct a (hrelabel b)))) c)


instance Braided Projection ((,,) x) where
  braid = Projection $ \(HProduct x (HProduct a b)) ->
    HProduct x (HProduct (hrelabel b) (hrelabel a))


instance Symmetric Projection ((,,) x)


instance PFunctor ((,,) x) Projection Projection where
  first f = bimap f id


instance QFunctor ((,,) x) Projection Projection where
  second = bimap id


instance Functor ((,,) x y) Projection Projection where
  fmap = second


instance Endofunctor ((,,) x y) Projection


instance Bifunctor ((,,,) x y) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $ \(HProduct xy (HProduct a b)) ->
    HProduct xy (HProduct (Label.hproject f a) (Label.hproject g b))


instance Associative Projection ((,,,) x y) where
  associate = Projection $ \(HProduct xy (HProduct x'y'ab c)) -> case hunlabel x'y'ab of
    HProduct x'y' (HProduct a b) ->
      HProduct xy (HProduct a (hlabel (HProduct x'y' (HProduct (hrelabel b) c))))

  disassociate = Projection $ \(HProduct xy (HProduct a x'y'bc)) -> case hunlabel x'y'bc of
    HProduct x'y' (HProduct b c) ->
      HProduct xy (HProduct (hlabel (HProduct x'y' (HProduct a (hrelabel b)))) c)


instance Braided Projection ((,,,) x y) where
  braid = Projection $ \(HProduct xy (HProduct a b)) ->
    HProduct xy (HProduct (hrelabel b) (hrelabel a))


instance Symmetric Projection ((,,,) x y)


instance PFunctor ((,,,) x y) Projection Projection where
  first f = bimap f id


instance QFunctor ((,,,) x y) Projection Projection where
  second = bimap id


instance Functor ((,,,) x y z) Projection Projection where
  fmap = second


instance Endofunctor ((,,,) x y z) Projection


instance Bifunctor ((,,,,) x y z) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $
    \(HProduct xy (HProduct z (HProduct a b))) ->
      HProduct xy $ HProduct z $
        HProduct (Label.hproject f a) (Label.hproject g b)


instance Associative Projection ((,,,,) x y z) where
  associate = Projection $
    \(HProduct xy (HProduct z (HProduct x'y'z'ab c))) ->
      case hunlabel x'y'z'ab of
        HProduct x'y' (HProduct z' (HProduct a b)) ->
          HProduct xy $ HProduct z $ HProduct a $ hlabel $
            HProduct x'y' $ HProduct z' $ HProduct (hrelabel b) c

  disassociate = Projection $
    \(HProduct xy (HProduct z (HProduct a x'y'z'bc))) ->
      case hunlabel x'y'z'bc of
        HProduct x'y' (HProduct z' (HProduct b c)) ->
          HProduct xy $ HProduct z $
            HProduct
              (hlabel (HProduct x'y' (HProduct z' (HProduct a (hrelabel b)))))
              c


instance Braided Projection ((,,,,) x y z) where
  braid = Projection $ \(HProduct xy (HProduct z (HProduct a b))) ->
    HProduct xy (HProduct z (HProduct (hrelabel b) (hrelabel a)))


instance Symmetric Projection ((,,,,) x y z)


instance PFunctor ((,,,,) x y z) Projection Projection where
  first f = bimap f id


instance QFunctor ((,,,,) x y z) Projection Projection where
  second = bimap id


instance Functor ((,,,,) x y z w) Projection Projection where
  fmap = second


instance Endofunctor ((,,,,) x y z w) Projection


instance Bifunctor ((,,,,,) x y z w) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $
    \(HProduct xyz (HProduct w (HProduct a b))) ->
      HProduct xyz $ HProduct w $
        HProduct (Label.hproject f a) (Label.hproject g b)


instance Associative Projection ((,,,,,) x y z w) where
  associate = Projection $
    \(HProduct xyz (HProduct w (HProduct x'y'z'w'ab c))) ->
      case hunlabel x'y'z'w'ab of
        HProduct x'y'z' (HProduct w' (HProduct a b)) ->
          HProduct xyz $ HProduct w $ HProduct a $ hlabel $
            HProduct x'y'z' $ HProduct w' $ HProduct (hrelabel b) c

  disassociate = Projection $
    \(HProduct xyz (HProduct w (HProduct a x'y'z'w'bc))) ->
      case hunlabel x'y'z'w'bc of
        HProduct x'y'z' (HProduct w' (HProduct b c)) ->
          HProduct xyz $ HProduct w $
            HProduct
              (hlabel (HProduct x'y'z' (HProduct w' (HProduct a (hrelabel b)))))
              c


instance Braided Projection ((,,,,,) x y z w) where
  braid = Projection $ \(HProduct xyz (HProduct w (HProduct a b))) ->
    HProduct xyz (HProduct w (HProduct (hrelabel b) (hrelabel a)))


instance Symmetric Projection ((,,,,,) x y z w)


instance PFunctor ((,,,,,) x y z w) Projection Projection where
  first f = bimap f id


instance QFunctor ((,,,,,) x y z w) Projection Projection where
  second = bimap id


instance Functor ((,,,,,) x y z w v) Projection Projection where
  fmap = second


instance Endofunctor ((,,,,,) x y z w v) Projection


instance Bifunctor ((,,,,,,) x y z w v) Projection Projection Projection where
  bimap (Projection f) (Projection g) = Projection $
    \(HProduct xyz (HProduct wv (HProduct a b))) ->
      HProduct xyz $ HProduct wv $
        HProduct (Label.hproject f a) (Label.hproject g b)


instance Associative Projection ((,,,,,,) x y z w v) where
  associate = Projection $
    \(HProduct xyz (HProduct wv (HProduct x'y'z'w'v'ab c))) ->
      case hunlabel x'y'z'w'v'ab of
        HProduct x'y'z' (HProduct w'v' (HProduct a b)) ->
          HProduct xyz $ HProduct wv $ HProduct a $ hlabel $
            HProduct x'y'z' $ HProduct w'v' $ HProduct (hrelabel b) c

  disassociate = Projection $
    \(HProduct xyz (HProduct wv (HProduct a x'y'z'w'v'bc))) ->
      case hunlabel x'y'z'w'v'bc of
        HProduct x'y'z' (HProduct w'v' (HProduct b c)) ->
          HProduct xyz $ HProduct wv $
            HProduct
              (hlabel (HProduct x'y'z' (HProduct w'v' (HProduct a (hrelabel b)))))
              c


instance Braided Projection ((,,,,,,) x y z w v) where
  braid = Projection $ \(HProduct xyz (HProduct wv (HProduct a b))) ->
    HProduct xyz (HProduct wv (HProduct (hrelabel b) (hrelabel a)))


instance Symmetric Projection ((,,,,,,) x y z w v)


instance PFunctor ((,,,,,,) x y z w v) Projection Projection where
  first f = bimap f id


instance QFunctor ((,,,,,,) x y z w v) Projection Projection where
  second = bimap id


instance Functor ((,,,,,,) x y z w v u) Projection Projection where
  fmap = second


instance Endofunctor ((,,,,,,) x y z w v u) Projection


-- | In the constraint @'Projectable' a' b' a b@, @a'@ and @b'@ are versions
-- of @a@ and @b@ that have been rewritten to be in a special 'Field' context.
type Projectable :: Type -> Type -> Type -> Type -> Constraint
class
  ( Recontextualize (Context a) (Field a) a a'
  , Recontextualize (Context a) (Field a) b b'
  )
  => Projectable a' b' a b | a -> a', a b -> b', a' b -> a, a b' -> b
instance
  ( Recontextualize (Context a) (Field a) a a'
  , Recontextualize (Context a) (Field a) b b'
  )
  => Projectable a' b' a b


-- | Turn a @'Projection' a b@ into a function @a -> b@.
project :: forall a b context. (Table context a, Table context b)
  => Projection a b -> a -> b
project (Projection f) = fromColumns . f . toColumns


-- | Construct a @'Projection' a b@ from a normal Haskell function. Note that
-- @a'@ and @b'@ have the same structure as @a@ and @b@, so you can use normal
-- pattern matching and record construction in the given function.
projection :: Projectable a' b' a b => (a' -> b') -> Projection a b
projection f = Projection $ \a -> case toColumns (f fields) of
  bs -> htabulate $ \field -> case hfield bs field of
    F (Field field') -> hfield a field'


-- | NOTE: Projection is not actually 'Monoidal', but categories got rid of
-- @PreCartesian@, so we now need a 'Monoidal' instance to use 'Cartesian'
instance Monoidal Projection (,) where
  type Id Projection (,) = TypeError
    ('Text "Projection is not actually Monoidal, but categories got rid of"
     ':$$:
     'Text "PreCartesian, so we need a Monoidal instance to use Cartesian")

  idl = Projection $ \(HProduct _ b) -> hunlabel b
  idr = Projection $ \(HProduct a _) -> hunlabel a
  coidl = Projection $ \a -> HProduct (hlabel notMonoidal) (hlabel a)
  coidr = Projection $ \a -> HProduct (hlabel a) (hlabel notMonoidal)


notMonoidal :: a
notMonoidal = error
  "Projection is not actually Monoidal, but categories got rid of\
  \ PreCartesian, so we now need a Monoidal instance to use Cartesian"
