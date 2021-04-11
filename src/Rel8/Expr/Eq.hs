{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Eq
  ( (==.), (/=.)
  , (==?), (/=?)
  , in_
  )
where

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( nonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (&&.), (||.), false, or_, coalesce )
import Rel8.Expr.Null ( isNull, unsafeLiftOpNull )
import Rel8.Expr.Opaleye ( fromPrimExpr, toPrimExpr, zipPrimExprsWith )
import Rel8.Schema.Null ( Nullity( NotNull, Null ), Sql, nullable )
import Rel8.Type.Eq ( DBEq )


eq :: DBEq a => Expr a -> Expr a -> Expr Bool
eq = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:==))


ne :: DBEq a => Expr a -> Expr a -> Expr Bool
ne = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:<>))


-- | Compare two expressions for equality. 
--
-- This corresponds to the SQL @IS NOT DISTINCT FROM@ operator, and will equate
-- @null@ values as @true@. This differs from @=@ which would return @null@.
-- This operator matches Haskell's '==' operator. For an operator identical to
-- SQL @=@, see '==?'.
(==.) :: forall a. Sql DBEq a => Expr a -> Expr a -> Expr Bool
(==.) = case nullable @a of
  Null -> \ma mb -> isNull ma &&. isNull mb ||. ma ==? mb
  NotNull -> eq
infix 4 ==.
{-# INLINABLE (==.) #-}


-- | Test if two expressions are different (not equal).
--
-- This corresponds to the SQL @IS DISTINCT FROM@ operator, and will return
-- @false@ when comparing two @null@ values. This differs from ordinary @=@
-- which would return @null@. This operator is closer to Haskell's '=='
-- operator. For an operator identical to SQL @=@, see '/=?'.
(/=.) :: forall a. Sql DBEq a => Expr a -> Expr a -> Expr Bool
(/=.) = case nullable @a of
  Null -> \ma mb -> isNull ma `ne` isNull mb ||. ma /=? mb
  NotNull -> ne
infix 4 /=.
{-# INLINABLE (/=.) #-}


-- | Test if two expressions are equal. This operator is usually the best
-- choice when forming join conditions, as PostgreSQL has a much harder time
-- optimizing a join that has multiple 'True' conditions.
--
-- This corresponds to the SQL @=@ operator, though it will always return a
-- 'Bool'.
(==?) :: DBEq a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a ==? b = coalesce $ unsafeLiftOpNull eq a b
infix 4 ==?


-- | Test if two expressions are different. 
--
-- This corresponds to the SQL @<>@ operator, though it will always return a
-- 'Bool'.
(/=?) :: DBEq a => Expr (Maybe a) -> Expr (Maybe a) -> Expr Bool
a /=? b = coalesce $ unsafeLiftOpNull ne a b
infix 4 /=?


-- | Like the SQL @IN@ operator, but implemented by folding over a list with
-- '==.' and '||.'.
in_ :: forall a f. (Sql DBEq a, Foldable f)
  => Expr a -> f (Expr a) -> Expr Bool
in_ a (toList -> as) = case nullable @a of
  Null -> or_ $ map (a ==.) as
  NotNull -> case nonEmpty as of
     Nothing -> false
     Just xs ->
       fromPrimExpr $
         Opaleye.BinExpr Opaleye.OpIn
           (toPrimExpr a)
           (Opaleye.ListExpr (toPrimExpr <$> xs))
