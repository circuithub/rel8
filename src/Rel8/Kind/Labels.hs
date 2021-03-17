{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeOperators #-}

module Rel8.Kind.Labels
  ( Labels
  , SLabels( SLabel, SLabels )
  , KnownLabels( labelsSing )
  , renderLabels
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ), (<|) )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude


type Labels :: Type
type Labels = [Symbol]


type SLabels :: Labels -> Type
data SLabels labels where
  SLabel :: KnownSymbol label => Proxy label -> SLabels '[label]
  SLabels :: KnownSymbol label => Proxy label -> SLabels labels -> SLabels (label ': labels)


type KnownLabels :: Labels -> Constraint
class KnownLabels labels where
  labelsSing :: SLabels labels


instance KnownSymbol label => KnownLabels (label ': '[]) where
  labelsSing = SLabel Proxy


instance (KnownSymbol label, KnownLabels (label_ ': labels)) =>
  KnownLabels (label ': (label_ ': labels))
 where
  labelsSing = SLabels Proxy labelsSing


renderLabels :: SLabels labels -> NonEmpty String
renderLabels = cleanup . \case
  SLabel label -> pure (symbolVal label)
  SLabels label labels -> symbolVal label <| renderLabels labels
  where
    cleanup ("" :| []) = "anon" :| []
    cleanup (a :| []) = a :| []
    cleanup (a :| [""]) = a :| []
    cleanup (a :| (b : c)) = a <| cleanup (b :| c)
