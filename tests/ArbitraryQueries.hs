{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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

module Main ( main ) where

import Data.Kind ( Type, Constraint )
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
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time 
import Data.Word (Word16, Word32, Word8)
import qualified Data.Text as StrictText
import qualified Data.Text.Lazy as LazyText
import Data.CaseInsensitive (CI, mk)
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.List (intersect)


data Dict :: Constraint -> Type where
  Dict :: c => Dict c


data Query :: Type -> Type -> Type where
  Values   :: [Table i o] -> Query i o
  UnionAll :: Query i o -> Query i o -> Query i o
  IntersectAll :: Query i o -> Query i o -> Query i o


deriving stock instance Show (Query i o)


genQuery :: TTable i o -> Gen (Query i o)
genQuery t@(isTableExpr -> Dict) = Gen.recursive Gen.choice 
  [ Values <$> Gen.list (Range.linear 0 10) (genTable t) 
  ]
  [ UnionAll <$> genQuery t <*> genQuery t 
  , IntersectAll <$> genQuery t <*> genQuery t 
  ]


compileQuery :: TTable i o -> Query i o -> Rel8.Query i
compileQuery t@(isTableExpr -> Dict) = \case
  Values xs -> Rel8.values $ compileTable t <$> xs
  UnionAll x y -> Rel8.unionAll (compileQuery t x) (compileQuery t y)
  IntersectAll x y -> Rel8.intersectAll (compileQuery t x) (compileQuery t y)


evalQuery :: Query i o -> [o]
evalQuery = \case
  Values xs -> evalTable <$> xs
  UnionAll x y -> evalQuery x ++ evalQuery y
  IntersectAll x y -> evalQuery x `intersect` evalQuery y

data Table :: Type -> Type -> Type where
  ExprTable :: Expr i o -> Table i o
  Product :: Table i1 o1 -> Table i2 o2 -> Table (i1, i2) (o1, o2)


deriving stock instance Show (Table i o)


genTable :: TTable i o -> Gen (Table i o)
genTable t = case (recursiveCases, nonRecursiveCases) of
  ([], ys) -> Gen.choice ys
  (xs, []) -> Gen.choice xs
  _        -> Gen.recursive Gen.choice nonRecursiveCases recursiveCases 
  where
    recursiveCases = case t of
      TProduct x y -> [ Product <$> genTable x <*> genTable y ]
      TExprTable _ -> []

    nonRecursiveCases = case t of
      TExprTable exprType -> [ ExprTable <$> genExpr exprType ]
      TProduct _ _ -> []


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
  LitN  :: Maybe (CanShow i) -> Expr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (Expr i o)


genExpr :: TExpr i o -> Gen (Expr i o)
genExpr = \case
  TNotNull dbType -> LitNN <$> genLiteral dbType
  TNull dbType    -> LitN <$> Gen.maybe (genLiteral dbType)


compileExpr :: TExpr i o -> Expr i o -> i
compileExpr t e = case (t, e) of
  (TNotNull (tdbtypeImplies -> Dict), LitNN x) -> Rel8.lit $ showing x
  (TNull (tdbtypeImplies -> Dict), LitN x)  -> Rel8.lit $ showing <$> x
  (TNull (tdbtypeImplies -> Dict), LitNN x)  -> Rel8.lit $ showing  x


evalExpr :: Expr i o -> o
evalExpr = \case
  LitNN l -> showing l
  LitN l -> showing <$> l


data CanShow a = Show a => CanShow { showing :: a }


deriving stock instance Show (CanShow a)


data Some :: (k -> Type) -> Type where
  Some :: k a -> Some k


data Uncurry :: (a -> b -> Type) -> (a, b) -> Type where
  Uncurry :: k a b -> Uncurry k '(a, b)


-- | Evidence of types that can be tables, and their corresponding Haskell type
-- on select.
data TTable :: Type -> Type -> Type where
  -- | An Expr is a table.
  TExprTable :: TExpr i o -> TTable i o
  TProduct :: TTable i1 o1 -> TTable i2 o2 -> TTable (i1, i2) (o1, o2)


deriving stock instance Show (TTable i o)


isTableExpr :: TTable i o -> Dict (Rel8.Table Rel8.Expr i)
isTableExpr = \case
  TExprTable (TNotNull (tdbtypeImplies -> Dict)) -> Dict
  TExprTable (TNull (tdbtypeImplies -> Dict)) -> Dict
  TProduct (isTableExpr -> Dict) (isTableExpr -> Dict) -> Dict


ttableEq :: TTable a b -> Dict (Eq b, Show b, Rel8.Serializable a b)
ttableEq t = case t of
  TExprTable (texprEq -> Dict) -> Dict
  TProduct (ttableEq -> Dict) (ttableEq -> Dict) -> Dict


genTTable :: Gen (Some (Uncurry TTable))
genTTable = Gen.recursive Gen.choice
  [ do genTExpr <&> \(Some (Uncurry t)) -> Some $ Uncurry $ TExprTable t
  ]
  [ Gen.subterm2 genTTable genTTable
      (\(Some (Uncurry x)) (Some (Uncurry y)) -> Some (Uncurry (TProduct x y))) 
  ]


data TExpr :: Type -> Type -> Type where
  -- | Not null expressions
  TNotNull :: TDBType i -> TExpr (Rel8.Expr i) i
  TNull :: TDBType i -> TExpr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (TExpr i o)


texprEq :: TExpr a b -> Dict (Eq b, Show b, Rel8.Serializable a b)
texprEq t = case t of
  TNotNull (tdbtypeImplies -> Dict) -> Dict
  TNull (tdbtypeImplies -> Dict) -> Dict


genTExpr :: Gen (Some (Uncurry TExpr))
genTExpr = choice
  [ genTDBType <&> \(Some t) -> Some $ Uncurry $ TNotNull t
  , genTDBType <&> \(Some t) -> Some $ Uncurry $ TNull t 
  ]


data TDBType :: Type -> Type where
  TBool            :: TDBType Bool
  TChar            :: TDBType Char
  TInt16           :: TDBType Int16
  TInt32           :: TDBType Int32
  TInt64           :: TDBType Int64
  TFloat           :: TDBType Float
  TDouble          :: TDBType Double
  TScientific      :: TDBType Scientific
  TUTCTime         :: TDBType UTCTime
  TDay             :: TDBType Day
  TLocalTime       :: TDBType LocalTime
  TTimeOfDay       :: TDBType TimeOfDay
  TDiffTime        :: TDBType DiffTime
  TNominalDiffTime :: TDBType DiffTime
  TText            :: TDBType StrictText.Text
  TLazyText        :: TDBType LazyText.Text
  TCIText          :: TDBType (CI StrictText.Text)
  TCILazyText      :: TDBType (CI LazyText.Text)
  TByteString      :: TDBType StrictByteString.ByteString
  TLazyByteString  :: TDBType LazyByteString.ByteString
  TUUID            :: TDBType UUID


deriving stock instance Show (TDBType a)


tdbtypeImplies :: TDBType a -> Dict (Eq a, Show a, Rel8.Serializable (Rel8.Expr a) a, Rel8.DBType a)
tdbtypeImplies = \case
  TBool            -> Dict
  TChar            -> Dict
  TInt16           -> Dict
  TInt32           -> Dict
  TInt64           -> Dict
  TFloat           -> Dict
  TDouble          -> Dict
  TScientific      -> Dict
  TUTCTime         -> Dict
  TDay             -> Dict
  TLocalTime       -> Dict
  TTimeOfDay       -> Dict
  TDiffTime        -> Dict
  TNominalDiffTime -> Dict
  TText            -> Dict
  TLazyText        -> Dict
  TCIText          -> Dict
  TCILazyText      -> Dict
  TByteString      -> Dict
  TLazyByteString  -> Dict
  TUUID            -> Dict


genTDBType :: Gen (Some TDBType)
genTDBType = Gen.element 
  [ Some TBool, Some TChar, Some TInt16, Some TInt32, Some TInt64, Some TFloat
  , Some TDouble, Some TScientific, Some TUTCTime, Some TDay, Some TLocalTime
  , Some TDiffTime, Some TNominalDiffTime, Some TText, Some TLazyText
  , Some TCIText, Some TCILazyText, Some TByteString, Some TLazyByteString
  , Some TUUID ]


genLiteral :: TDBType a -> Gen (CanShow a)
genLiteral = \case
  TBool       -> CanShow <$> Gen.element [True, False]
  TChar       -> CanShow <$> Gen.unicode
  TInt16      -> CanShow <$> Gen.integral Range.linearBounded
  TInt32      -> CanShow <$> Gen.integral Range.linearBounded
  TInt64      -> CanShow <$> Gen.integral Range.linearBounded
  TFloat      -> CanShow <$> Gen.float (fromIntegral <$> Range.linearBounded @Int32)
  TDouble     -> CanShow <$> Gen.double (fromIntegral <$> Range.linearBounded @Int32)
  TScientific -> CanShow <$> Gen.realFrac_ (fromIntegral <$> Range.linearBounded @Int32)

  TUTCTime    -> CanShow <$> do
    UTCTime 
      <$> (showing <$> genLiteral TDay) 
      <*> do fromIntegral @Int32 <$> Gen.integral (Range.linear 0 86401)

  TDay        -> CanShow <$> do
    Gen.just $ fromGregorianValid 
      <$> Gen.integral (Range.linear 1970 3000) 
      <*> Gen.integral (Range.linear 1 12) 
      <*> Gen.integral (Range.linear 1 31)

  TLocalTime -> CanShow <$> do
    LocalTime <$> do showing <$> genLiteral TDay
              <*> do showing <$> genLiteral TTimeOfDay

  TTimeOfDay -> CanShow <$> do
    Gen.just $ 
      makeTimeOfDayValid 
        <$> Gen.integral (Range.linear 0 23) 
        <*> Gen.integral (Range.linear 0 59) 
        <*> do fromInteger <$> Gen.integral (Range.linear 0 60)

  TDiffTime -> CanShow <$> do
    fromIntegral <$> Gen.integral (Range.linearBounded @Int32)

  TNominalDiffTime -> CanShow <$> do
    fromIntegral <$> Gen.integral (Range.linearBounded @Int32)

  TText -> CanShow <$> Gen.text (Range.linear 0 512) Gen.unicode
  TLazyText -> CanShow . LazyText.fromStrict . showing <$> genLiteral TText

  TCIText -> CanShow . mk . showing <$> genLiteral TText
  TCILazyText -> CanShow . mk . showing <$> genLiteral TLazyText

  TByteString -> CanShow . StrictByteString.pack <$> Gen.list (Range.linear 0 512) (Gen.integral (Range.linearBounded @Word8))
  TLazyByteString -> CanShow . LazyByteString.pack <$> Gen.list (Range.linear 0 512) (Gen.integral (Range.linearBounded @Word8))

  TUUID -> CanShow <$> do 
    UUID.fromWords
      <$> Gen.integral Range.linearBounded 
      <*> Gen.integral Range.linearBounded
      <*> Gen.integral Range.linearBounded
      <*> Gen.integral Range.linearBounded


main :: IO ()
main =
  defaultMain $
  withResource startTestDatabase stopTestDatabase \getTestDatabase ->
  withResource (connect getTestDatabase) release \getC ->
  testProperty "Random queries" $ property do
    Opaque (Some (Uncurry t)) <- forAll $ Opaque <$> genTTable
    annotateShow t

    q <- forAll (genQuery t)

    case ttableEq t of 
      Dict -> test do
        annotate $ Rel8.showQuery $ compileQuery t q
        c <- liftIO getC
        results <- evalIO $ Rel8.select c $ compileQuery t q
        results === evalQuery q

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
