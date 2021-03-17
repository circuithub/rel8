{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Table.Bool ( ifThenElse_, case_ ) where

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( toPrimExpr ), fromPrimExpr )
import Rel8.HTable ( HTable( hfield, htabulate ) )
import Rel8.Table ( Columns, Table, fromColumns, toColumns )


-- | Branch two expressions based on a predicate. Similar to @if ... then ...
-- else@ in Haskell (and implemented using @CASE@ in SQL). 
-- 
-- >>> select c (return (ifThenElse_ (lit True) (lit "True!") (lit "False!") :: Expr Text))
-- ["True!"]
-- 
-- Note that unlike SQL, this function can be used to return multiple columns:
-- 
-- >>> import Data.Text (pack)
-- >>> :{
-- select c $ pure $
--   ifThenElse_ 
--     (lit False) 
--     (lit (pack "A", pack "B")) 
--     (lit (pack "C", pack "D"))
-- :}
-- [("C","D")]
ifThenElse_ :: Table Expr a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue = case_ [(bool, whenTrue)]


case_ :: forall a. Table Expr a => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  fromColumns $ htabulate @(Columns a) \x -> fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ hfield (toColumns alt) x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ hfield (toColumns def) x )
