{-# language KindSignatures #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Expr ( Expr(Expr) ) where

-- base
import Data.Kind ( Type )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- | Typed SQL expressions
type Expr :: Type -> Type
newtype Expr a = Expr { toPrimExpr :: Opaleye.PrimExpr }
