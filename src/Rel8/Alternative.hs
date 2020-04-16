{-# language FlexibleContexts #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Alternative where

import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity )
import Data.Indexed.Functor.Representable ( HRepresentable )
import Data.Indexed.Functor.Traversable ( HTraversable )
import Data.Kind ( Type )
import Data.Tagged.PolyKinded ( Tagged(..) )
import Rel8.Column ( Column )


-- | The work-horse of Rel8. This is what users would derive.
class (HRepresentable (Schema a), HTraversable (Schema a)) => Relational (a :: Type) where
  type Schema (a :: Type) = (f :: (Type -> Type) -> Type) | f -> a

  from :: a -> Schema a Identity
  to :: Schema a Identity -> a


-- | Something is a Row if it is isomorphic to a functor-indexed type indexed by
-- Column. This is a roundabout way of saying a row is just a product of
-- columns.
class (HRepresentable (RowSchema a), HTraversable (RowSchema a)) => Row (a :: Type) where
  type RowSchema a = (f :: (Type -> Type) -> Type) | f -> a
  toColumns :: a -> RowSchema a Column
  fromColumns :: RowSchema a Column -> a


-- | Anything 'Relational' is a 'Row', as witnessed by 'LiftRow'.
newtype LiftRow a = LiftRow (Schema a Column)


instance Relational a => Row (LiftRow a) where
  type RowSchema (LiftRow a) = Compose (Tagged (LiftRow a)) (Schema a)

  toColumns = coerce
  fromColumns = coerce


-- | Types that we can 'Select' are types where we can decode their columns into
-- a Haskell type, and where we can encode the Haskell type as a product of
-- columns.
class (Row columns, Relational haskell) => Select columns haskell | columns -> haskell, haskell -> columns where
  decode :: RowSchema columns Column -> Schema haskell Identity
  encode :: Schema haskell Identity -> RowSchema columns Column

