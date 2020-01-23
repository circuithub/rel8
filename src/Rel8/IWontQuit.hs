{-# language BlockArguments #-}
{-# language QuantifiedConstraints #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.IWontQuit where

import Data.Kind

-- data Expr ( m :: * -> * ) a =
--   Expr

-- type family DropLeadingMaybes ( a :: * ) :: * where
--   DropLeadingMaybes ( Maybe a ) = DropLeadingMaybes a
--   DropLeadingMaybes a = a

-- type family Column ( f :: * -> * ) ( a :: * ) :: * where
--   Column ( Expr m ) a = Expr m a
--   Column ( Null f ) a = Column f ( Maybe ( DropLeadingMaybes a ) )

-- newtype C f a =
--   C { toColumn :: Column f a }

-- data Null ( f :: * -> * ) a

-- class Table ( t :: Type ) where
--   data Field t ( a :: Type )

--   type TableContext t :: * -> *

--   index :: t -> Field t a -> C ( TableContext t ) a

--   tabulate :: ( forall x. Field t x -> C ( TableContext t ) x ) -> t


-- class ( Table ( Rewrite t f ), TableContext ( Rewrite t f ) ~ TableContext t ) => Rewritable t ( f :: ( * -> * ) -> * -> * ) where
--   type Rewrite t f = ( t' :: * ) | t' -> t f

--   rewrite :: t -> Rewrite t f


data MaybeTable t =
  MaybeTable { isNull :: Column ( TableContext t ) Bool
             , maybeTable :: Rewrite t Null
             }


instance Rewritable t Null => Table ( MaybeTable t ) where
  data Field ( MaybeTable t ) a where
    IsNullField :: Field ( MaybeTable t ) Bool
    MaybeTableField :: Field ( Rewrite t Null ) a -> Field ( MaybeTable t ) a

  type TableContext ( MaybeTable t ) =
    TableContext t

  index MaybeTable{ isNull, maybeTable } = \case
    IsNullField -> C isNull
    MaybeTableField i -> index maybeTable i

  tabulate f =
    MaybeTable { isNull = toColumn ( f IsNullField )
               , maybeTable = tabulate ( f . MaybeTableField )
               }


toMaybeTable :: ( TableContext t ~ Expr m, Table t, Rewritable t Null ) => t -> MaybeTable t
toMaybeTable t =
  tabulate \case
    IsNullField -> C Expr
    MaybeTableField i ->
      case index ( rewrite t ) i of
        C Expr -> C Expr


data Part f =
  Part { partId :: Column f Int
       , partNull :: Column f ( Maybe Bool )
       }


instance Table ( Part ( Expr m ) ) where
  data Field ( Part ( Expr m ) ) a where
    PartIdExpr :: Field ( Part ( Expr m ) ) Int
    PartNullExpr :: Field ( Part ( Expr m ) ) ( Maybe Bool )

  type TableContext ( Part ( Expr m ) ) =
    Expr m

  index Part{ partId, partNull } = \case
    PartIdExpr -> C partId
    PartNullExpr -> C partNull

  tabulate f =
    Part { partId = toColumn ( f PartIdExpr )
         , partNull = toColumn ( f PartNullExpr )
         }

instance TableContext ( Part f ) ~ f => Table ( Part ( Null f ) ) where
  type TableContext ( Part ( Null f ) ) =
    TableContext ( Part f )

  data Field ( Part ( Null f ) ) a where
    NullPartId :: Field ( Part ( Null f ) ) ( Maybe Int )
    NullPartNull :: Field ( Part ( Null f ) ) ( Maybe Bool )

  index Part{ partId, partNull } = \case
    NullPartId -> C partId
    NullPartNull -> C partNull

  tabulate f =
    Part { partId = toColumn ( f NullPartId )
         , partNull = toColumn ( f NullPartNull )
         }

class ApplyRewrite ( g :: ( * -> * ) -> * -> * ) ( f :: * -> * ) where
  rewriteC :: C f a -> C ( g f ) a

instance ApplyRewrite Null ( Expr m ) where
  rewriteC ( C Expr ) =
    C Expr

instance ( ApplyRewrite g f, Table ( Part ( g f ) ), TableContext ( Part ( g f ) ) ~ TableContext ( Part f ) ) => Rewritable ( Part f ) g where
  type Rewrite ( Part f ) g =
    Part ( g f )

  rewrite Part{ partId, partNull } =
    Part { partId = toColumn ( rewriteC @g ( C @f @Int partId ) )
         , partNull = toColumn ( rewriteC @g ( C @f @( Maybe Bool ) partNull ) )
         }


maybePart :: MaybeTable ( Part ( Expr IO ) )
maybePart =
  toMaybeTable Part{ partId = Expr
                   , partNull = Expr
                   }


-- test :: _
-- test = ( partNull ( maybeTable maybePart )
--        , partId ( maybeTable maybePart )
--        )
