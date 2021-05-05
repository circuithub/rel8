{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeOperators #-}

module Rel8.Kind.Labels
  ( Labels
  , SLabels( SNil, SCons )
  , KnownLabels( labelsSing )
  , renderLabels
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Data.Maybe ( fromMaybe )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude


type Labels :: Type
type Labels = [Symbol]


type SLabels :: Labels -> Type
data SLabels labels where
  SNil :: SLabels '[]
  SCons :: KnownSymbol label => Proxy label -> SLabels labels -> SLabels (label ': labels)


type KnownLabels :: Labels -> Constraint
class KnownLabels labels where
  labelsSing :: SLabels labels


instance KnownLabels '[] where
  labelsSing = SNil


instance (KnownSymbol label, KnownLabels labels) =>
  KnownLabels (label ': labels)
 where
  labelsSing = SCons Proxy labelsSing


renderLabels :: SLabels labels -> NonEmpty String
renderLabels = fromMaybe (pure "anon") . nonEmpty . go
  where
    go :: SLabels labels -> [String]
    go = \case
      SNil -> []
      SCons label labels -> symbolVal label : go labels
