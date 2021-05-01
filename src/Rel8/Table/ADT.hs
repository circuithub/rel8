{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.ADT
  ( ADT( ADT )
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GRep, GColumns, GContext, gfromColumns, gtoColumns
  , GColumnsADT, gfromColumnsADT, gtoColumnsADT
  , greify, gunreify
  )
import qualified Rel8.Kind.Algebra as K
import Rel8.Schema.Context ( Col )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Reify ( hreify, hunreify )


type ADT :: K.Rel8able -> K.Rel8able
newtype ADT t context = ADT (GColumnsADT t (Col context))


instance Rel8able t => Rel8able (ADT t) where
  type Algebra (ADT t) = 'K.Product

  type GRep (ADT t) context = GRep t context

  type GColumns (ADT t) = GColumnsADT t
  type GContext (ADT t) context = context

  gfromColumns = ADT
  gtoColumns (ADT a) = a

  gfromColumnsADT = ADT
  gtoColumnsADT (ADT a) = a

  greify (ADT a) = ADT (hreify a)
  gunreify (ADT a) = ADT (hunreify a)
