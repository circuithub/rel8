{-# language ApplicativeDo #-}
{-# language LambdaCase #-}

{-# options -fno-warn-name-shadowing #-}

module Rel8.Optimize where

import Control.Applicative
import Data.Functor.Identity
import Opaleye.Internal.HaskellDB.PrimQuery
import Opaleye.Internal.PrimQuery


primQuery
  :: Applicative f
  => ( PrimQuery' a -> f ( PrimQuery' a ) ) -> PrimQuery' a -> f ( PrimQuery' a )
primQuery f = \case
  Product primQueries bindings -> do
    primQueries <- traverse f primQueries

    return ( Product primQueries bindings )

  Aggregate bindingsAndPrimExpr primQuery -> do
    primQuery <- f primQuery

    return ( Aggregate bindingsAndPrimExpr primQuery )

  DistinctOnOrderBy maybePrimExprs orderExprs primQuery -> do
    primQuery <- f primQuery

    return ( DistinctOnOrderBy maybePrimExprs orderExprs primQuery )

  Limit limitOp primQuery -> do
    primQuery <- f primQuery

    return ( Limit limitOp primQuery )

  Join joinType primExpr bindingsA bindingsB primQueryA primQueryB -> do
    primQueryA <- f primQueryA
    primQueryB <- f primQueryB

    return ( Join joinType primExpr bindingsA bindingsB primQueryA primQueryB )

  Exists bool primQueryA primQueryB -> do
    primQueryA <- f primQueryA
    primQueryB <- f primQueryB

    return ( Exists bool primQueryA primQueryB )

  Binary binOp bindings primQueries -> do
    primQueries <- both f primQueries

    return ( Binary binOp bindings primQueries  )

  Label label primQuery -> do
    primQuery <- f primQuery

    return ( Label label primQuery )

  other ->
    pure other


optimisePredicates :: PrimQuery' a -> PrimQuery' a
optimisePredicates =
  rewriteOf primQuery optimisePrimQuery

  where

    optimisePrimQuery = \case
      Product primQueries predicates ->
        Just ( Product primQueries ( map nullIsFalse predicates ) )

      _ ->
        Nothing


nullIsFalse :: PrimExpr -> PrimExpr
nullIsFalse =
  rewriteOf primExprs simplifyCaseAnalysis

  where

    simplifyCaseAnalysis = \case
      CaseExpr [ ( UnExpr OpIsNull _, ConstExpr ( BoolLit False ) ) ] notNullBranch ->
        Just notNullBranch

      _ ->
        Nothing



-- | Traverse all immediate 'PrimExpr's
primExprs :: Applicative f => ( PrimExpr -> f PrimExpr ) -> PrimExpr -> f PrimExpr
primExprs f = \case
  AttrExpr ( Symbol string tag ) ->
    pure ( AttrExpr ( Symbol string tag ) )

  BaseTableAttrExpr attribute ->
    pure ( BaseTableAttrExpr attribute )

  CompositeExpr primExpr attribute -> do
    primExpr <- f primExpr

    return ( CompositeExpr primExpr attribute )

  BinExpr binOp a b -> do
    a <- f a
    b <- f b

    return ( BinExpr binOp a b )

  UnExpr unOp primExpr -> do
    primExpr <- f primExpr

    return ( UnExpr unOp primExpr )

  AggrExpr aggrDistinct aggrOp primExpr orderExprs -> do
    aggrOp <- aggrOpPrimExprs f aggrOp
    primExpr <- f primExpr
    orderExprs <- traverse ( orderExprPrimExprs f ) orderExprs

    return ( AggrExpr aggrDistinct aggrOp primExpr orderExprs )

  ConstExpr l ->
    pure ( ConstExpr l )

  CaseExpr cases def -> do
    cases <- traverse ( both f ) cases
    def <- f def

    return ( CaseExpr cases def )

  other ->
    pure other



aggrOpPrimExprs :: Applicative f => ( PrimExpr -> f PrimExpr ) -> AggrOp -> f AggrOp
aggrOpPrimExprs f = \case
  AggrStringAggr primExpr -> do
    primExpr <- f primExpr

    return ( AggrStringAggr primExpr )

  other ->
    pure other


orderExprPrimExprs :: Applicative f => ( PrimExpr -> f PrimExpr ) -> OrderExpr -> f OrderExpr
orderExprPrimExprs f ( OrderExpr orderOp primExpr ) = do
  primExpr <- f primExpr

  return ( OrderExpr orderOp primExpr )


-- | Traverse both sides of a homogeneous tuple.
both :: Applicative f => ( t -> f b ) -> ( t, t ) -> f ( b, b )
both f ( a,b ) =
  liftA2 (,) ( f a ) ( f b )


rewriteOf
  :: ( ( a -> Identity a ) -> a -> Identity a )
  -> ( a -> Maybe a ) -> a -> a
rewriteOf l f =
  go where

  go = transformOf l ( \x -> maybe x go ( f x ) )


transformOf
  :: ( ( a -> Identity a ) -> a -> Identity a )
  -> ( a -> a )
  -> a
  -> a
{-# inline transformOf #-}
transformOf l f = go where

  go =
    f . over l go


over
  :: ( ( s -> Identity t ) -> a -> Identity b )
  -> ( s -> t )
  -> a
  -> b
over l f =
  runIdentity . l (Identity . f)
