{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Structure
  ( Structure
  , Shape( Column, Either, List, Maybe, NonEmpty, These )
  , Shape1
  , Shape2
  , IsStructure
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Schema.Spec ( Context, Spec )


type Structure :: Context
data Structure spec


type Shape :: Type
data Shape
  = Column Spec
  | Either Type Type
  | List Type
  | Maybe Type
  | NonEmpty Type
  | These Type Type


type Shape1 :: (a -> Shape) -> a -> Type
data Shape1 shape a


type Shape2 :: (a -> b -> Shape) -> a -> b -> Type
data Shape2 shape a b


type IsStructure :: Type -> Bool
type family IsStructure a where
  IsStructure (Shape1 _ _) = 'True
  IsStructure (Shape2 _ _ _) = 'True
  IsStructure (_, _) = 'True
  IsStructure (_, _, _) = 'True
  IsStructure (_, _, _, _) = 'True
  IsStructure (_, _, _, _, _) = 'True
  IsStructure _ = 'False
