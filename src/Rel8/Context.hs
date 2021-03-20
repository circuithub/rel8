{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

{-# options -Wno-duplicate-exports #-}

module Rel8.Context
  ( Context( Column )
  , Meta( Meta )
  , Column( I, ColumnDecoder, ComposedColumn )
  , Defaulting(..)
  , decompose
  , fromColumnDecoder
  , unI
  ) where

-- base
import Data.Functor.Compose ( Compose )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )

-- hasql
import qualified Hasql.Decoders as Hasql


data Meta = Meta Defaulting Type


data Defaulting = HasDefault | NoDefault


class Context (f :: Type -> Type) where
  data Column f :: Meta -> Type


instance Context Identity where
  data Column Identity :: Meta -> Type where
    I :: { unI :: a } -> Column Identity ('Meta defaulting a)


instance Context Hasql.Row where
  data Column Hasql.Row :: Meta -> Type where
    ColumnDecoder :: { fromColumnDecoder :: Hasql.Row a } -> Column Hasql.Row ('Meta defaulting a)


instance Context (Compose f g) where
  newtype Column (Compose f g) :: Meta -> Type where
    ComposedColumn :: { decompose :: g (Column f x) } -> Column (Compose f g) x
