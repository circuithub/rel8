{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

{-# options -Wno-duplicate-exports #-}

module Rel8.Context
  ( Context( Column )
  , Meta( Meta )
  , Column( I, ColumnDecoder, ComposedColumn )
  , decompose
  , fromColumnDecoder
  , unI
  ) where

-- 
import qualified Hasql.Decoders as Hasql

-- base
import Data.Functor.Compose ( Compose )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )


newtype Meta = Meta Type


class Context (f :: Type -> Type) where
  data Column f :: Meta -> Type


instance Context Identity where
  data Column Identity :: Meta -> Type where
    I :: { unI :: a } -> Column Identity ('Meta a)


instance Context Hasql.Row where
  data Column Hasql.Row :: Meta -> Type where
    ColumnDecoder :: { fromColumnDecoder :: Hasql.Row a } -> Column Hasql.Row ('Meta a)


instance Context (Compose f g) where
  newtype Column (Compose f g) :: Meta -> Type where
    ComposedColumn :: { decompose :: g (Column f x) } -> Column (Compose f g) x
