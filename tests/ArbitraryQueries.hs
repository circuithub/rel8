{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Main where

-- import Hedgehog
import Data.Kind ( Type )
import qualified Rel8
import Hedgehog
import Hedgehog.Gen (choice)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Database.Postgres.Temp as TmpPostgres
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (acquire, release)
import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Hasql.Session (run, sql)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty (defaultMain, withResource)
import Data.Functor ((<&>))
import Control.Applicative (liftA2)


data Query :: Type -> Type -> Type where
  Values :: Rel8.Table Rel8.Expr i => [Table i o] -> Query i o
  UnionAll :: Rel8.Table Rel8.Expr i => Query i o -> Query i o -> Query i o

deriving stock instance Show (Query i o)


compileQuery :: TTable i o -> Query i o -> Rel8.Query i
compileQuery t = \case
  Values xs -> Rel8.values $ compileTable t <$> xs
  UnionAll x y -> Rel8.unionAll (compileQuery t x) (compileQuery t y)


evalQuery :: Query i o -> [o]
evalQuery = \case
  Values xs -> evalTable <$> xs
  UnionAll x y -> evalQuery x ++ evalQuery y


data Table :: Type -> Type -> Type where
  ExprTable :: Expr i o -> Table i o
  Product :: Table i1 o1 -> Table i2 o2 -> Table (i1, i2) (o1, o2)


deriving stock instance Show (Table i o)


compileTable :: TTable i o -> Table i o -> i
compileTable t e = case (t, e) of
  (TExprTable t', ExprTable expr) -> compileExpr t' expr
  (TProduct x' y', Product x y) -> (compileTable x' x, compileTable y' y)


evalTable :: Table i o -> o
evalTable = \case
  ExprTable expr -> evalExpr expr
  (Product x y) -> (evalTable x, evalTable y)


data Expr :: Type -> Type -> Type where
  LitNN :: CanShow i -> Expr (Rel8.Expr i) i
  LitN :: Maybe (CanShow i) -> Expr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (Expr i o)


compileExpr :: TExpr i o -> Expr i o -> i
compileExpr t e = case (t, e) of
  (TNotNull t', LitNN x) -> isDBType t' $ Rel8.lit $ showing x
  (TNull t', LitN x)  -> isDBType t' $ Rel8.lit $ showing <$> x


evalExpr :: Expr i o -> o
evalExpr = \case
  LitNN l -> showing l
  LitN l -> showing <$> l


data CanShow a = Show a => CanShow { showing :: a }


deriving stock instance Show (CanShow a)


genQuery :: TTable i o -> Gen (Query i o)
genQuery t = choice 
  [ isTableExpr t $
      Values <$> Gen.list (Range.linear 0 10) (genTable t) 
  , isTableExpr t $ 
      Gen.subterm2 (genQuery t) (genQuery t) UnionAll
  ]


genTable :: TTable i o -> Gen (Table i o)
genTable = \case
  TExprTable exprType -> ExprTable <$> genExpr exprType
  TProduct x y -> Product <$> genTable x <*> genTable y


genExpr :: TExpr i o -> Gen (Expr i o)
genExpr = \case
  TNotNull dbType -> LitNN <$> genLiteral dbType
  TNull dbType    -> LitN <$> Gen.maybe (genLiteral dbType)


genLiteral :: TDBType a -> Gen (CanShow a)
genLiteral = \case
  TBool -> CanShow <$> Gen.element [True, False]


-- | Evidence of types that can be tables, and their corresponding Haskell type
-- on select.
data TTable :: Type -> Type -> Type where
  -- | An Expr is a table.
  TExprTable :: TExpr i o -> TTable i o
  TProduct :: TTable i1 o1 -> TTable i2 o2 -> TTable (i1, i2) (o1, o2)


deriving stock instance Show (TTable i o)


ttableEq :: TTable a b -> ((Eq b, Show b, Rel8.Serializable a b) => r) -> r
ttableEq t = case t of
  TExprTable e -> texprEq e
  TProduct x y -> \k -> ttableEq y $ ttableEq x k


texprEq :: TExpr a b -> ((Eq b, Show b, Rel8.Serializable a b) => r) -> r
texprEq t k = case t of
  TNotNull TBool -> k
  TNull TBool -> k


data Some :: (k -> Type) -> Type where
  Some :: k a -> Some k


data Uncurry :: (a -> b -> Type) -> (a, b) -> Type where
  Uncurry :: k a b -> Uncurry k '(a, b)


genTTable :: Gen (Some (Uncurry TTable))
genTTable = choice
  [ genTExpr <&> \(Some (Uncurry t)) -> Some $ Uncurry $ TExprTable t 
  , Gen.subterm2 genTTable genTTable
      (\(Some (Uncurry x)) (Some (Uncurry y)) -> Some (Uncurry (TProduct x y)))
  ]


genTExpr :: Gen (Some (Uncurry TExpr))
genTExpr = choice
  [ genTDBType <&> \(Some t) -> Some $ Uncurry $ TNotNull t
  , genTDBType <&> \(Some t) -> Some $ Uncurry $ TNull t 
  ]


genTDBType :: Gen (Some TDBType)
genTDBType = pure $ Some TBool



data TExpr :: Type -> Type -> Type where
  -- | Not null expressions
  TNotNull :: TDBType i -> TExpr (Rel8.Expr i) i
  TNull :: TDBType i -> TExpr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (TExpr i o)


data TDBType :: Type -> Type where
  TBool :: TDBType Bool


deriving stock instance Show (TDBType a)


isTableExpr :: TTable i o -> (Rel8.Table Rel8.Expr i => r) -> r
isTableExpr = \case
  TExprTable (TNotNull dbType) -> isDBType dbType
  TExprTable (TNull dbType) -> isDBType dbType
  TProduct x y -> \k -> isTableExpr x $ isTableExpr y k


isDBType :: TDBType o -> (Rel8.DBType o => r) -> r
isDBType = \case
  TBool -> id


main :: IO ()
main =
  defaultMain $
  withResource startTestDatabase stopTestDatabase \getTestDatabase ->
  withResource (connect getTestDatabase) release \getC ->
  testProperty "Random queries" $ property do
    Opaque (Some (Uncurry t)) <- forAll $ Opaque <$> genTTable
    annotateShow t
    liftIO $ print t

    q <- forAll (genQuery t)

    ttableEq t $ test do
      c <- liftIO getC
      results <- liftIO $ Rel8.select c $ compileQuery t q
      results === evalQuery q
      liftIO $ putStrLn "OK"

  where

    connect getTestDatabase = 
      either (error . show) return =<< 
        acquire . TmpPostgres.toConnectionString =<< 
          getTestDatabase

    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (either (error . show) return =<< acquire (TmpPostgres.toConnectionString db)) release \conn -> void do
        flip run conn do
          sql "CREATE EXTENSION citext"
          sql "CREATE TABLE test_table ( column1 text not null, column2 bool not null )"

      return db

    stopTestDatabase = TmpPostgres.stop
