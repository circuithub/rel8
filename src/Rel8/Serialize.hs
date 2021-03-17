module Rel8.Serialize where

type Serialize :: Type -> Type -> Constraint
class Serialize expr haskell | expr -> haskell where
  lit 
