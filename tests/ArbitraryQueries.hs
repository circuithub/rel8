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

import Data.List.NonEmpty ( NonEmpty )
import Data.Function ( (&) )
import Control.Applicative ( liftA2 )
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
import Data.Word (Word8)
import qualified Data.Text as StrictText
import qualified Data.Text.Lazy as LazyText
import Data.CaseInsensitive (CI, mk)
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.List (intersect)
import qualified Data.Set as Set
import Data.Containers.ListUtils ( nubOrd )
import Data.Char (ord)


data Dict :: Constraint -> Type where
  Dict :: c => Dict c


data Query :: Type -> Type -> Type where
  Values       :: (Rel8.Table Rel8.Expr i, Ord o) => [Table i o] -> Query i o
  Union        :: (Rel8.EqTable i, Ord o)         => Query i o -> Query i o -> Query i o
  UnionAll     :: (Rel8.Table Rel8.Expr i, Ord o) => Query i o -> Query i o -> Query i o
  Intersect    :: (Rel8.EqTable i, Ord o)         => Query i o -> Query i o -> Query i o
  IntersectAll :: (Rel8.EqTable i, Ord o)         => Query i o -> Query i o -> Query i o
  Except       :: (Rel8.EqTable i, Ord o)         => Query i o -> Query i o -> Query i o
  ExceptAll    :: (Rel8.EqTable i, Ord o)         => Query i o -> Query i o -> Query i o
  Many         :: (Rel8.Table Rel8.Expr i, Ord o) => Query i o -> Query (Rel8.ListTable i) [o]
  Some         :: (Rel8.Table Rel8.Expr i, Ord o) => Query i o -> Query (Rel8.NonEmptyTable i) (NonEmpty o)
  Optional     :: (Rel8.Table Rel8.Expr i, Ord o) => Query i o -> Query (Rel8.MaybeTable i) (Maybe o)


deriving stock instance Show (Query i o)


genQuery :: TTable i o -> Gen (Query i o)
genQuery t@(isTableExpr -> Dict) = Gen.recursive Gen.choice 
  [ Values <$> Gen.list (Range.linear 0 10) (genTable t) 
  ]
  (concat
    [ [ UnionAll <$> genQuery t <*> genQuery t 
      ]
    , case t of
        TListTable t'@(isTableExpr -> Dict) -> 
          [ Many <$> genQuery t' ]

        TNonEmptyTable t'@(isTableExpr -> Dict) -> 
          [ Some <$> genQuery t' ]

        TMaybeTable t'@(isTableExpr -> Dict) -> 
          [ Optional <$> genQuery t' ]

        _ -> []
    , isEqTable t & foldMap \Dict ->
        [ Union <$> genQuery t <*> genQuery t 
        , Intersect <$> genQuery t <*> genQuery t 
        , IntersectAll <$> genQuery t <*> genQuery t 
        , Except <$> genQuery t <*> genQuery t 
        , ExceptAll <$> genQuery t <*> genQuery t 
        ]
    ]
  )


compileQuery :: Query i o -> Rel8.Query i
compileQuery = \case
  Values xs        -> Rel8.values $ compileTable <$> xs
  Union x y        -> Rel8.union (compileQuery x) (compileQuery y)
  UnionAll x y     -> Rel8.unionAll (compileQuery x) (compileQuery y)
  Intersect x y    -> Rel8.intersect (compileQuery x) (compileQuery y)
  IntersectAll x y -> Rel8.intersectAll (compileQuery x) (compileQuery y)
  Except x y       -> Rel8.except (compileQuery x) (compileQuery y)
  ExceptAll x y    -> Rel8.exceptAll (compileQuery x) (compileQuery y)
  Many q           -> Rel8.many (compileQuery q)
  Some q           -> Rel8.some (compileQuery q)
  Optional q       -> Rel8.optional (compileQuery q)


evalQuery :: Ord o => Query i o -> [o]
evalQuery = \case
  Values xs -> evalTable <$> xs
  Union x y -> nubOrd $ evalQuery x ++ evalQuery y
  UnionAll x y -> evalQuery x ++ evalQuery y
  Intersect x y -> Set.toList $ Set.fromList (evalQuery x) `Set.intersection` Set.fromList (evalQuery y)
  IntersectAll x y -> intersect (evalQuery x) (evalQuery y)
  Except x y -> Set.toList $ Set.fromList (evalQuery x) `Set.difference` Set.fromList (evalQuery y)
  ExceptAll x y -> filter (`notElem` ys) (evalQuery x)
    where ys = evalQuery y

  Many q -> [evalQuery q]
  Optional q -> case evalQuery q of
                  [] -> [Nothing]
                  xs -> Just <$> xs


data Table :: Type -> Type -> Type where
  ExprTable :: Expr i o -> Table i o
  Product :: Table i1 o1 -> Table i2 o2 -> Table (i1, i2) (o1, o2)


deriving stock instance Show (Table i o)


genTable :: TTable i o -> Gen (Table i o)
genTable t = case (recursiveCases, nonRecursiveCases) of
  ([], []) -> Gen.discard
  ([], ys) -> Gen.choice ys
  (xs, []) -> Gen.choice xs
  _        -> Gen.recursive Gen.choice nonRecursiveCases recursiveCases 
  where
    recursiveCases = case t of
      TProduct x y -> [ Product <$> genTable x <*> genTable y ]
      TExprTable _ -> []
      TListTable t -> []
      TNonEmptyTable t -> []
      TMaybeTable t -> []

    nonRecursiveCases = case t of
      TExprTable exprType -> [ ExprTable <$> genExpr exprType ]
      TProduct _ _ -> []
      TListTable _ -> []
      TNonEmptyTable t -> []
      TMaybeTable t -> []


compileTable :: Table i o -> i
compileTable = \case
  ExprTable expr -> compileExpr expr
  Product x y -> (compileTable x, compileTable y)


evalTable :: Table i o -> o
evalTable = \case
  ExprTable expr -> evalExpr expr
  (Product x y) -> (evalTable x, evalTable y)


data Expr :: Type -> Type -> Type where
  LitNN :: Rel8.DBType i => CanShow i -> Expr (Rel8.Expr i) i
  LitN  :: Rel8.DBType i => Maybe (CanShow i) -> Expr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (Expr i o)


genExpr :: TExpr i o -> Gen (Expr i o)
genExpr = \case
  TNotNull t@(tdbtypeImplies -> Dict) -> LitNN <$> genLiteral t
  TNull t@(tdbtypeImplies -> Dict)    -> LitN <$> Gen.maybe (genLiteral t)


compileExpr :: Expr i o -> i
compileExpr = \case
  LitNN x -> Rel8.lit $ showing x
  LitN x  -> Rel8.lit $ showing <$> x


evalExpr :: Expr i o -> o
evalExpr = \case
  LitNN l -> showing l
  LitN l -> showing <$> l


data CanShow a = Show a => CanShow { showing :: a }


deriving stock instance Show (CanShow a)


data Exists :: (k -> Type) -> Type where
  Exists :: k a -> Exists k


data Uncurry :: (a -> b -> Type) -> (a, b) -> Type where
  Uncurry :: k a b -> Uncurry k '(a, b)


-- | Evidence of types that can be tables, and their corresponding Haskell type
-- on select.
data TTable :: Type -> Type -> Type where
  -- | An Expr is a table.
  TExprTable :: TExpr i o -> TTable i o
  TProduct :: TTable i1 o1 -> TTable i2 o2 -> TTable (i1, i2) (o1, o2)
  TListTable :: TTable i o -> TTable (Rel8.ListTable i) [o]
  TNonEmptyTable :: TTable i o -> TTable (Rel8.NonEmptyTable i) (NonEmpty o)
  TMaybeTable :: TTable i o -> TTable (Rel8.MaybeTable i) (Maybe o)


deriving stock instance Show (TTable i o)


isTableExpr :: TTable i o -> Dict (Rel8.Table Rel8.Expr i, Ord o, Rel8.Serializable i o, Show o)
isTableExpr = \case
  TExprTable (TNotNull (tdbtypeImplies -> Dict)) -> Dict
  TExprTable (TNull (tdbtypeImplies -> Dict)) -> Dict
  TProduct (isTableExpr -> Dict) (isTableExpr -> Dict) -> Dict
  TListTable (isTableExpr -> Dict) -> Dict
  TNonEmptyTable (isTableExpr -> Dict) -> Dict
  TMaybeTable (isTableExpr -> Dict) -> Dict


isEqTable :: TTable i o -> Maybe (Dict (Rel8.EqTable i))
isEqTable = \case
  TExprTable (TNotNull dbType) -> isDBEq dbType <&> \Dict -> Dict
  TExprTable (TNull dbType)    -> isDBEq dbType <&> \Dict -> Dict
  TProduct x y                 -> liftA2 (\Dict Dict -> Dict) (isEqTable x) (isEqTable y)
  TListTable t -> Nothing
  TNonEmptyTable t -> Nothing
  TMaybeTable (isTableExpr -> Dict) -> Nothing



genTTable :: Gen (Exists (Uncurry TTable))
genTTable = Gen.recursive Gen.choice
  [ do genTExpr <&> \(Exists (Uncurry t)) -> Exists $ Uncurry $ TExprTable t
  ]
  [ Gen.subterm2 genTTable genTTable
      (\(Exists (Uncurry x)) (Exists (Uncurry y)) -> Exists (Uncurry (TProduct x y))) 
  ]


data TExpr :: Type -> Type -> Type where
  -- | Not null expressions
  TNotNull :: TDBType i -> TExpr (Rel8.Expr i) i
  TNull :: TDBType i -> TExpr (Rel8.Expr (Maybe i)) (Maybe i)


deriving stock instance Show (TExpr i o)


texprEq :: TExpr a b -> Dict (Ord b, Show b, Rel8.Serializable a b)
texprEq t = case t of
  TNotNull (tdbtypeImplies -> Dict) -> Dict
  TNull (tdbtypeImplies -> Dict) -> Dict


genTExpr :: Gen (Exists (Uncurry TExpr))
genTExpr = choice
  [ genTDBType <&> \(Exists t) -> Exists $ Uncurry $ TNotNull t
  , genTDBType <&> \(Exists t) -> Exists $ Uncurry $ TNull t 
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


tdbtypeImplies :: TDBType a -> Dict (Ord a, Show a, Rel8.Serializable (Rel8.Expr a) a, Rel8.DBType a)
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


isDBEq :: TDBType a -> Maybe (Dict (Rel8.DBEq a))
isDBEq = \case
  TBool            -> Just Dict
  TChar            -> Just Dict
  TInt16           -> Just Dict
  TInt32           -> Just Dict
  TInt64           -> Just Dict
  TFloat           -> Just Dict
  TDouble          -> Just Dict
  TScientific      -> Just Dict
  TUTCTime         -> Just Dict
  TDay             -> Just Dict
  TLocalTime       -> Just Dict
  TTimeOfDay       -> Just Dict
  TDiffTime        -> Just Dict
  TNominalDiffTime -> Just Dict
  TText            -> Just Dict
  TLazyText        -> Just Dict
  TCIText          -> Just Dict
  TCILazyText      -> Just Dict
  TByteString      -> Just Dict
  TLazyByteString  -> Just Dict
  TUUID            -> Just Dict


genTDBType :: Gen (Exists TDBType)
genTDBType = Gen.element 
  [ Exists TBool, Exists TChar, Exists TInt16, Exists TInt32, Exists TInt64
  , Exists TFloat , Exists TDouble, Exists TScientific, Exists TUTCTime
  , Exists TDay, Exists TLocalTime , Exists TDiffTime, Exists TNominalDiffTime
  , Exists TText, Exists TLazyText , Exists TCIText, Exists TCILazyText
  , Exists TByteString, Exists TLazyByteString , Exists TUUID ]


genLiteral :: TDBType a -> Gen (CanShow a)
genLiteral = \case
  TBool       -> CanShow <$> Gen.element [True, False]
  TChar       -> CanShow <$> Gen.filter (\c -> ord c > 0) Gen.unicode
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
    Opaque (Exists (Uncurry t')) <- forAll $ Opaque <$> genTTable
    annotateShow t'

    let t = TListTable t'

    q <- forAll (genQuery t)

    case isTableExpr t of 
      Dict -> test do
        let query = compileQuery q
        annotate $ Rel8.showQuery query
        c <- liftIO getC
        results <- evalIO $ Rel8.select c query
        return ()
        -- results === evalQuery q

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
