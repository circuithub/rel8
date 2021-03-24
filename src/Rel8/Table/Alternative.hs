{-# language FlexibleContexts #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Table.Alternative
  ( AltTable ( (<|>:) )
  , AlternativeTable ( emptyTable )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Table ( Table )


type AltTable :: (Type -> Type) -> Constraint
class AltTable f where
  (<|>:) :: Table Expr a => f a -> f a -> f a
  infixl 3 <|>:


type AlternativeTable :: (Type -> Type) -> Constraint
class AltTable f => AlternativeTable f where
  emptyTable :: Table Expr a => f a
