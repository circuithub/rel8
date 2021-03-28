{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import qualified Data.Map.Lazy as LazyMap
import Data.These ( These(..) )
import Data.List.NonEmpty ( NonEmpty(..) )
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
import Data.Char (ord)
import Data.Maybe (catMaybes)
import Data.Type.Equality
import qualified Data.List.NonEmpty as NonEmpty
import Data.List (permutations)
import Data.Foldable (foldl', fold)
import qualified Data.Set as Set
import Data.Containers.ListUtils (nubOrd)


type Context = [(Type, Type)]


data ATypedQuery :: Context -> Type where
  ATypedQuery :: (Show o, Rel8.Serializable i o, Ord o) 
    => TypedQuery env i o 
    -> ATypedQuery env


deriving stock instance Show (ATypedQuery env)


data TypedQuery env i o = TypedQuery 
  { queryType :: TTable i o
  , query :: Query env i o 
  } 
  deriving stock (Show)


-- | A reified 'Query' value.
data Query :: Context -> Type -> Type -> Type where
  Values :: (Rel8.Table Rel8.Expr i, Ord o)
    => [Table env i o] -> Query env i o

  Union :: Rel8.EqTable i                    
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  UnionAll :: Rel8.Table Rel8.Expr i            
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  Intersect :: Rel8.EqTable i                    
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  IntersectAll :: Rel8.EqTable i                    
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  Except :: Rel8.EqTable i
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  ExceptAll :: Rel8.EqTable i                    
    => TypedQuery env i o -> TypedQuery env i o -> Query env i o

  Limit :: ()                                
    => Word -> TypedQuery env i o -> Query env i o

  Offset :: ()                                
    => Word -> TypedQuery env i o -> Query env i o

  WhereExists :: Show y                          
    => Table env i o -> TypedQuery env x y -> Query env i o

  WhereNotExists  :: Show y                          
    => Table env i o -> TypedQuery env x y -> Query env i o

  Many :: (Rel8.Table Rel8.Expr i, Show o)  
    => TypedQuery env i o -> Query env (Rel8.ListTable i) [o]

  Some :: (Rel8.Table Rel8.Expr i, Show o)  
    => TypedQuery env i o -> Query env (Rel8.NonEmptyTable i) (NonEmpty o)

  Optional :: (Rel8.Table Rel8.Expr i, Show o)  
    => TypedQuery env i o -> Query env (Rel8.MaybeTable i) (Maybe o)

  Distinct :: (Rel8.EqTable i)                  
    => TypedQuery env i o -> Query env i o

  Bind :: (Show y, Ord y) 
    => TypedQuery env x y -> TypedQuery ('(x, y) ': env) i o -> Query env i o

  ReturnTable :: ()
    => Table env i o -> Query env i o


deriving stock instance Show o => Show (Query env i o)


-- | Extend a query with a new 'Bind'. The new 'Bind' has access to an updated
-- environment with the result of any previous binds.
bindQuery :: (Functor m, Show y, Ord y)
  => Environment env 
  -> TypedQuery env x y 
  -> (forall env'. Environment env' -> m (TypedQuery env' i o))
  -> m (TypedQuery env i o)
bindQuery env TypedQuery{ query, queryType = t } f = 
  case query of
    Bind x y ->
      bindQuery (Extend (queryType x) env) y f <&> \y' ->
        TypedQuery (queryType y') $ Bind x y'

    q -> 
      f (Extend t env) <&> \y ->
        TypedQuery (queryType y) $ Bind (TypedQuery t q) y


data Environment :: Context -> Type where
  Empty :: Environment '[]
  Extend :: TTable i o -> Environment env -> Environment ('(i, o) ': env)


genQueryOfType :: Environment env -> TTable i o -> Gen (TypedQuery env i o)
genQueryOfType env tableType = do
  ATypedQuery x <- genTypedQuery env
  bindQuery env x (fmap (TypedQuery tableType . ReturnTable) . genTable tableType)


genTypedQuery :: Environment env -> Gen (ATypedQuery env)
genTypedQuery env = Gen.recursive Gen.choice nonrecursive recursive
  where
    nonrecursive = [ values, returnTable ]
      where
        values = do
          ATTable tableType <- genTTable
          tables <- Gen.list (Range.linear 0 10) (genTable tableType env)
          return $ ATypedQuery $ TypedQuery tableType $ Values tables

        returnTable = do
          ATTable tableType <- genTTable
          t <- genTable tableType env 
          return $ ATypedQuery $ TypedQuery tableType $ ReturnTable t

    recursive = 
      [ bind, union, unionAll, intersect, intersectAll, except, exceptAll
      , limit, offset, whereExists, whereNotExists, many, some, optional
      , distinct
      ]
      where
        bind = do
          ATypedQuery x <- genTypedQuery env
          ATypedQuery y <- genTypedQuery (Extend (queryType x) env)
          return $ ATypedQuery $ TypedQuery (queryType y) $ Bind x y

        union = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ Union x y

        unionAll = do
          ATTable t <- genTTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ UnionAll x y

        intersect = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ Intersect x y

        intersectAll = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ IntersectAll x y

        except = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ Except x y

        exceptAll = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$>
            Gen.subterm2 (genQueryOfType env t) (genQueryOfType env t) \x y -> 
              TypedQuery t $ ExceptAll x y

        limit =
          Gen.subtermM (genTypedQuery env) \(ATypedQuery q) -> do
            n <- genLimitOffset
            return $ ATypedQuery $ TypedQuery (queryType q) $ Limit n q

        offset = do
          Gen.subtermM (genTypedQuery env) \(ATypedQuery q) -> do
            n <- genLimitOffset
            return $ ATypedQuery $ TypedQuery (queryType q) $ Offset n q

        whereExists = do
          ATypedQuery q <- genTypedQuery env 

          ATTable tableType <- genTTable
          table <- genTable tableType env

          return $ ATypedQuery $ TypedQuery tableType $ WhereExists table q

        whereNotExists = do
          ATypedQuery q <- genTypedQuery env 

          ATTable tableType <- genTTable
          table <- genTable tableType env

          return $ ATypedQuery $ TypedQuery tableType $ WhereNotExists table q

        many = do
          ATypedQuery q <- genTypedQuery env 
          return $ ATypedQuery $ TypedQuery (TListTable (queryType q)) $ Many q

        some = do
          ATypedQuery q <- genTypedQuery env 
          return $ ATypedQuery $ TypedQuery (TNonEmptyTable (queryType q)) $ Some q

        optional = do
          ATypedQuery q <- genTypedQuery env 
          return $ ATypedQuery $ TypedQuery (TMaybeTable (queryType q)) $ Optional q

        distinct = do
          ATEqTable t <- genTEqTable
          ATypedQuery <$> Gen.subterm (genQueryOfType env t) (TypedQuery t . Distinct)

    genLimitOffset :: Gen Word
    genLimitOffset = 
      Gen.integral (Range.linear 0 ((maxBound `div` 2) - 1))


data Results :: Context -> Type where
  NoResults :: Results '[]
  Store :: i -> Results env -> Results ('(i, o) ': env)


compileQuery :: Results env -> TypedQuery env i o -> Rel8.Query i
compileQuery env TypedQuery{ query } = 
  case query of
    Bind x f             -> compileQuery env x >>= \res -> compileQuery (Store res env) f
    ReturnTable x        -> return $ compileTable env x

    Values xs            -> Rel8.values $ compileTable env <$> xs
    Union x y            -> Rel8.union (compileQuery env x) (compileQuery env y)
    UnionAll x y         -> Rel8.unionAll (compileQuery env x) (compileQuery env y)
    Intersect x y        -> Rel8.intersect (compileQuery env x) (compileQuery env y)
    IntersectAll x y     -> Rel8.intersectAll (compileQuery env x) (compileQuery env y)
    Except x y           -> Rel8.except (compileQuery env x) (compileQuery env y)
    ExceptAll x y        -> Rel8.exceptAll (compileQuery env x) (compileQuery env y)
    Limit n q            -> Rel8.limit n (compileQuery env q)
    Offset n q           -> Rel8.offset n (compileQuery env q)
    WhereExists ret q    -> compileTable env ret <$ Rel8.whereExists (compileQuery env q)
    WhereNotExists ret q -> compileTable env ret <$ Rel8.whereExists (compileQuery env q)
    Many q               -> Rel8.many (compileQuery env q)
    Some q               -> Rel8.some (compileQuery env q)
    Optional q           -> Rel8.optional (compileQuery env q)
    Distinct q           -> Rel8.distinct (compileQuery env q)


data Table :: Context -> Type -> Type -> Type where
  ExprTable :: ()                 
    => Expr env i o -> Table env i o

  PairTable :: (Show o1, Show o2) 
    => Table env i1 o1 -> Table env i2 o2 -> Table env (i1, i2) (o1, o2)

  Lookup :: ()                 
    => TableIx env i o -> Table env i o

  JustTable :: (Show o)           
    => Table env i o -> Table env (Rel8.MaybeTable i) (Maybe o)

  NothingTable :: Rel8.Table Rel8.Expr i 
    => Table env (Rel8.MaybeTable i) (Maybe o)

  LeftTable :: (Rel8.Table Rel8.Expr x, Show o) 
    => Table env i o -> Table env (Rel8.EitherTable i x) (Either o y)

  RightTable :: (Rel8.Table Rel8.Expr x, Show o)
    => Table env i o -> Table env (Rel8.EitherTable x i) (Either y o)

  ThisTable :: (Rel8.Table Rel8.Expr x, Show o) 
    => Table env i o -> Table env (Rel8.TheseTable i x) (These o y)

  ThatTable :: (Rel8.Table Rel8.Expr x, Show o)           
    => Table env i o -> Table env (Rel8.TheseTable x i) (These y o)

  ThoseTable :: (Rel8.Table Rel8.Expr i1, Rel8.Table Rel8.Expr i2, Show o1, Show o2)
    => Table env i1 o1 
    -> Table env i2 o2 
    -> Table env (Rel8.TheseTable i1 i2) (These o1 o2)

  ListTable :: (Rel8.Table Rel8.Expr i, Show o)
    => [Table env i o]
    -> Table env (Rel8.ListTable i) [o]

  NonEmptyTable :: (Rel8.Table Rel8.Expr i, Show o)
    => NonEmpty (Table env i o)
    -> Table env (Rel8.NonEmptyTable i) (NonEmpty o)


deriving stock instance Show o => Show (Table env i o)


data TableIx :: Context -> Type -> Type -> Type where
  Here :: TableIx ('(i, o) ': env) i o
  There :: TableIx env i o -> TableIx (x ': env) i o


deriving stock instance Show (TableIx env i o)


findIxs :: TTable i o -> Environment env -> [TableIx env i o]
findIxs _ Empty = []
findIxs t (Extend t' env) =
  case eqTTable t t' of
    Just Refl -> Here : map There (findIxs t env)
    Nothing -> map There (findIxs t env)


genTableIx :: TTable i o -> Environment env -> Maybe (Gen (TableIx env i o))
genTableIx t env = 
  case findIxs t env of
    [] -> Nothing
    xs -> Just $ Gen.element xs


-- | Generate a 'Table Expr' term for a particular table type.
genTable :: TTable i o -> Environment env -> Gen (Table env i o)
genTable t env = Gen.choice $ catMaybes 
  [ exprTable, pairTable, existingTable, justTable, nothingTable, leftTable
  , rightTable, thisTable, thatTable, thoseTable, listTable, nonEmptyTable
  ]
  where
    exprTable = do
      TTableExpr exprType <- pure t
      return do
        ExprTable <$> genExpr env exprType

    pairTable = do
      TTablePair x y <- pure t
      return do
        PairTable <$> genTable x env <*> genTable y env

    existingTable = do
      genIx <- genTableIx t env 
      return $ Lookup <$> genIx

    justTable = do
      TMaybeTable t' <- pure t
      return $ JustTable <$> genTable t' env

    nothingTable = do
      TMaybeTable _ <- pure t
      return $ pure NothingTable

    leftTable = do
      TEitherTable x _ <- pure t
      return $ LeftTable <$> genTable x env

    rightTable = do
      TEitherTable _ y <- pure t
      return $ RightTable <$> genTable y env

    thisTable = do
      TTheseTable x _ <- pure t
      return $ ThisTable <$> genTable x env

    thatTable = do
      TTheseTable _ y <- pure t
      return $ ThatTable <$> genTable y env

    thoseTable = do
      TTheseTable x y <- pure t
      return $ ThoseTable <$> genTable x env <*> genTable y env

    listTable = do
      TListTable t' <- pure t
      return do
        xs <- Gen.list (Range.linear 0 10) (genTable t' env)
        return $ ListTable xs

    nonEmptyTable = do
      TNonEmptyTable t' <- pure t
      return do
        xs <- 
          Gen.just $ 
            NonEmpty.nonEmpty <$> 
              Gen.list (Range.linear 1 10) (genTable t' env)
        return $ NonEmptyTable xs


find :: TableIx env i o -> Results env -> i
find Here       (Store x _)   = x
find (There ix) (Store _ env) = find ix env


compileTable :: Results env -> Table env i o -> i
compileTable env = \case
  ExprTable expr   -> compileExpr env expr
  PairTable x y    -> (compileTable env x, compileTable env y)
  Lookup ix        -> find ix env
  JustTable t      -> Rel8.justTable (compileTable env t)
  NothingTable     -> Rel8.nothingTable
  LeftTable t      -> Rel8.leftTable (compileTable env t)
  RightTable t     -> Rel8.rightTable (compileTable env t)
  ThisTable t      -> Rel8.thisTable (compileTable env t)
  ThatTable t      -> Rel8.thatTable (compileTable env t)
  ThoseTable x y   -> Rel8.thoseTable (compileTable env x) (compileTable env y)
  ListTable xs     -> Rel8.listTable $ compileTable env <$> xs
  NonEmptyTable xs -> Rel8.nonEmptyTable $ compileTable env <$> xs


data Expr :: Context -> Type -> Type -> Type where
  LitNN :: Rel8.DBType a
    => a -> Expr env (Rel8.Expr a) a

  LitN :: Rel8.DBType a 
   => Maybe a -> Expr env (Rel8.Expr (Maybe a)) (Maybe a)

  IsJustTable :: Show o        
    => Table env (Rel8.MaybeTable i) (Maybe o) -> Expr env (Rel8.Expr Bool) Bool

  IsNothingTable :: Show o        
    => Table env (Rel8.MaybeTable i) (Maybe o) -> Expr env (Rel8.Expr Bool) Bool

  IsLeftTable :: (Show o1, Show o2)
    => Table env (Rel8.EitherTable i1 i2) (Either o1 o2) -> Expr env (Rel8.Expr Bool) Bool

  IsRightTable :: (Show o1, Show o2)
    => Table env (Rel8.EitherTable i1 i2) (Either o1 o2) -> Expr env (Rel8.Expr Bool) Bool


deriving stock instance Show o => Show (Expr env i o)


genExpr :: Environment env -> TExpr i o -> Gen (Expr env i o)
genExpr env t = Gen.recursive Gen.choice nonrecursive recursive
  where
    recursive = catMaybes [ isJustTable, isNothingTable, isLeftTable, isRightTable ]
      where
        isJustTable = do
          TNotNull TBool <- pure t
          pure do
            ATTable t' <- genTTable
            IsJustTable <$> genTable (TMaybeTable t') env

        isNothingTable = do
          TNotNull TBool <- pure t
          pure do
            ATTable t' <- genTTable
            IsJustTable <$> genTable (TMaybeTable t') env

        isLeftTable = do
          TNotNull TBool <- pure t
          pure do
            ATTable t1 <- genTTable
            ATTable t2 <- genTTable
            IsLeftTable <$> genTable (TEitherTable t1 t2) env

        isRightTable = do
          TNotNull TBool <- pure t
          pure do
            ATTable t1 <- genTTable
            ATTable t2 <- genTTable
            IsRightTable <$> genTable (TEitherTable t1 t2) env

    nonrecursive = catMaybes [ notNullLiteral, nullLiteral ]
      where
        notNullLiteral = do
          TNotNull tdbType <- pure t
          pure do
            LitNN <$> genLiteral tdbType

        nullLiteral =  do
          TNull tdbType <- pure t
          pure do
            LitN <$> Gen.maybe (genLiteral tdbType)


compileExpr :: Results env -> Expr env i o -> i
compileExpr env = \case
  LitNN x          -> Rel8.lit x
  LitN x           -> Rel8.lit x
  IsJustTable t    -> Rel8.isJustTable (compileTable env t)
  IsNothingTable t -> Rel8.isNothingTable (compileTable env t)
  IsLeftTable t    -> Rel8.isLeftTable (compileTable env t)
  IsRightTable t   -> Rel8.isRightTable (compileTable env t)


data ATTable :: Type where
  ATTable :: (Rel8.Table Rel8.Expr i, Show o, Rel8.Serializable i o, Ord o) 
    => TTable i o -> ATTable


data ATEqTable :: Type where
  ATEqTable :: (Rel8.EqTable i, Show o, Rel8.Serializable i o, Ord o) 
    => TTable i o -> ATEqTable


-- | Types that can be tables. For any 'TTable i o', we know 'Table Expr i'
-- and 'Serializable i o'.
data TTable :: Type -> Type -> Type where
  TTableExpr     :: ()                 
    => TExpr (Rel8.Expr i) i -> TTable (Rel8.Expr i) i

  TTablePair :: (Show o1, Show o2) 
    => TTable i1 o1 -> TTable i2 o2 -> TTable (i1, i2) (o1, o2)

  TListTable :: (Rel8.Table Rel8.Expr i, Show o)
    => TTable i o -> TTable (Rel8.ListTable i) [o]

  TNonEmptyTable :: (Rel8.Table Rel8.Expr i, Show o)
    => TTable i o -> TTable (Rel8.NonEmptyTable i) (NonEmpty o)

  TMaybeTable :: (Show o, Rel8.Table Rel8.Expr i) 
    => TTable i o -> TTable (Rel8.MaybeTable i) (Maybe o)

  TTheseTable :: (Rel8.Table Rel8.Expr i1, Rel8.Table Rel8.Expr i2, Show o1, Show o2)
    => TTable i1 o1 -> TTable i2 o2 -> TTable (Rel8.TheseTable i1 i2) (These o1 o2)

  TEitherTable :: (Show o1, Show o2, Rel8.Table Rel8.Expr i1, Rel8.Table Rel8.Expr i2) 
    => TTable i1 o1 -> TTable i2 o2 -> TTable (Rel8.EitherTable i1 i2) (Either o1 o2)


deriving stock instance Show (TTable i o)


eqTTable :: TTable i o -> TTable i' o' -> Maybe ((i, o) :~: (i', o'))
eqTTable x y =
  case (x, y) of
    (TTableExpr e1, TTableExpr e2) -> 
      eqTExpr e1 e2

    (TTablePair x1 y1, TTablePair x2 y2) -> do
      Refl <- eqTTable x1 x2
      Refl <- eqTTable y1 y2
      return Refl

    (TListTable a, TListTable b) -> do
      Refl <- eqTTable a b
      return Refl

    (TNonEmptyTable a, TNonEmptyTable b) -> do
      Refl <- eqTTable a b
      return Refl

    (TMaybeTable a, TMaybeTable b) -> do
      Refl <- eqTTable a b
      return Refl

    (TTheseTable x1 y1, TTheseTable x2 y2) -> do
      Refl <- eqTTable x1 x2
      Refl <- eqTTable y1 y2
      return Refl
      
    (TEitherTable x1 y1, TEitherTable x2 y2) -> do
      Refl <- eqTTable x1 x2
      Refl <- eqTTable y1 y2
      return Refl

    _ ->
      Nothing


-- | Generate a type that is an instance of 'Table Expr'.
genTTable :: Gen ATTable
genTTable = Gen.recursive Gen.choice nonrecursive recursive
  where
    nonrecursive = [ exprAsTable ]
      where
        exprAsTable :: Gen ATTable
        exprAsTable = do
          ATExpr exprType <- genTExpr
          return $ ATTable $ TTableExpr exprType

    recursive = [ pair, eitherTable, theseTable ]
      where
        pair =
          Gen.subterm2 genTTable genTTable \(ATTable x) (ATTable y) ->
            ATTable $ TTablePair x y

        eitherTable =
          Gen.subterm2 genTTable genTTable \(ATTable x) (ATTable y) ->
            ATTable $ TEitherTable x y

        theseTable =
          Gen.subterm2 genTTable genTTable \(ATTable x) (ATTable y) ->
            ATTable $ TTheseTable x y


genTEqTable :: Gen ATEqTable
genTEqTable = Gen.recursive Gen.choice nonrecursive recursive
  where
    nonrecursive = [ exprAsTable ]
      where
        exprAsTable :: Gen ATEqTable
        exprAsTable = do
          ATEqExpr exprType <- genTEqExpr
          return $ ATEqTable $ TTableExpr exprType

    recursive = [ pair, eitherTable, maybeTable, theseTable, listTable, nonEmptyTable ]
      where
        pair = 
          Gen.subterm2 genTEqTable genTEqTable \(ATEqTable x) (ATEqTable y) ->
            ATEqTable $ TTablePair x y

        eitherTable = 
          Gen.subterm2 genTEqTable genTEqTable \(ATEqTable x) (ATEqTable y) ->
            ATEqTable $ TEitherTable x y

        maybeTable = 
          Gen.subterm genTEqTable \(ATEqTable x) ->
            ATEqTable $ TMaybeTable x

        theseTable = 
          Gen.subterm2 genTEqTable genTEqTable \(ATEqTable x) (ATEqTable y) ->
            ATEqTable $ TTheseTable x y

        listTable = 
          Gen.subterm genTEqTable \(ATEqTable x) ->
            ATEqTable $ TListTable x

        nonEmptyTable = 
          Gen.subterm genTEqTable \(ATEqTable x) ->
            ATEqTable $ TNonEmptyTable x


data ATExpr :: Type where
  ATExpr :: (Show a, Rel8.Sql Rel8.DBType a, Ord a) 
    => TExpr (Rel8.Expr a) a -> ATExpr


data ATEqExpr :: Type where
  ATEqExpr :: (Show a, Rel8.Sql Rel8.DBEq a, Ord a) 
    => TExpr (Rel8.Expr a) a -> ATEqExpr


data TExpr :: Type -> Type -> Type where
  TNotNull :: Rel8.DBType a 
    => TDBType a -> TExpr (Rel8.Expr a) a

  TNull :: Rel8.DBType a 
    => TDBType a -> TExpr (Rel8.Expr (Maybe a)) (Maybe a)


deriving stock instance Show (TExpr i o)


eqTExpr :: TExpr i1 o1 -> TExpr i2 o2 -> Maybe ((i1, o1) :~: (i2, o2))
eqTExpr x y =
  case (x, y) of
    (TNotNull a, TNotNull b) -> do
      Refl <- eqDBType a b
      return Refl

    (TNull a, TNull b) -> do
      Refl <- eqDBType a b
      return Refl

    _ ->
      Nothing


genTExpr :: Gen ATExpr
genTExpr = choice
  [ genTDBType <&> \(ATDBType t) -> ATExpr $ TNotNull t
  , genTDBType <&> \(ATDBType t) -> ATExpr $ TNull t 
  ]


genTEqExpr :: Gen ATEqExpr
genTEqExpr = choice
  [ genTDBEq <&> \(ATDBEq t) -> ATEqExpr $ TNotNull t
  , genTDBEq <&> \(ATDBEq t) -> ATEqExpr $ TNull t 
  ]


data ATDBType :: Type where
  ATDBType :: (Rel8.DBType a, Show a, Ord a) => TDBType a -> ATDBType


data ATDBEq :: Type where
  ATDBEq :: (Rel8.DBEq a, Show a, Ord a) => TDBType a -> ATDBEq


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


eqDBType :: TDBType a -> TDBType b -> Maybe (a :~: b)
eqDBType x y =
  case (x, y) of
    (TBool, TBool)                       -> Just Refl
    (TChar, TChar)                       -> Just Refl
    (TInt16, TInt16)                     -> Just Refl
    (TInt32, TInt32)                     -> Just Refl
    (TInt64, TInt64)                     -> Just Refl
    (TFloat, TFloat)                     -> Just Refl
    (TDouble, TDouble)                   -> Just Refl
    (TScientific, TScientific)           -> Just Refl
    (TUTCTime, TUTCTime)                 -> Just Refl
    (TDay, TDay)                         -> Just Refl
    (TLocalTime, TLocalTime)             -> Just Refl
    (TTimeOfDay, TTimeOfDay)             -> Just Refl
    (TDiffTime, TDiffTime)               -> Just Refl
    (TNominalDiffTime, TNominalDiffTime) -> Just Refl
    (TText, TText)                       -> Just Refl
    (TLazyText, TLazyText)               -> Just Refl
    (TCIText, TCIText)                   -> Just Refl
    (TCILazyText, TCILazyText)           -> Just Refl
    (TByteString, TByteString)           -> Just Refl
    (TLazyByteString, TLazyByteString)   -> Just Refl
    (TUUID, TUUID)                       -> Just Refl
    _                                    -> Nothing


genTDBType :: Gen ATDBType
genTDBType = Gen.element 
  [ ATDBType TBool, ATDBType TChar, ATDBType TInt16, ATDBType TInt32
  , ATDBType TInt64, ATDBType TFloat, ATDBType TDouble, ATDBType TScientific
  , ATDBType TUTCTime, ATDBType TDay, ATDBType TLocalTime, ATDBType TDiffTime 
  , ATDBType TNominalDiffTime, ATDBType TText, ATDBType TLazyText
  , ATDBType TCIText, ATDBType TCILazyText, ATDBType TByteString
  , ATDBType TLazyByteString, ATDBType TUUID 
  ]


genTDBEq :: Gen ATDBEq
genTDBEq = Gen.element 
  [ ATDBEq TBool, ATDBEq TChar, ATDBEq TInt16, ATDBEq TInt32
  , ATDBEq TInt64, ATDBEq TFloat, ATDBEq TDouble, ATDBEq TScientific
  , ATDBEq TUTCTime, ATDBEq TDay, ATDBEq TLocalTime, ATDBEq TDiffTime 
  , ATDBEq TNominalDiffTime, ATDBEq TText, ATDBEq TLazyText
  , ATDBEq TCIText, ATDBEq TCILazyText, ATDBEq TByteString
  , ATDBEq TLazyByteString, ATDBEq TUUID 
  ]


genLiteral :: TDBType a -> Gen a
genLiteral = \case
  TBool       -> Gen.element [True, False]
  TChar       -> Gen.filter (\c -> ord c > 0) Gen.unicode
  TInt16      -> Gen.integral Range.linearBounded
  TInt32      -> Gen.integral Range.linearBounded
  TInt64      -> Gen.integral Range.linearBounded
  TFloat      -> Gen.float (fromIntegral <$> Range.linearBounded @Int32)
  TDouble     -> Gen.double (fromIntegral <$> Range.linearBounded @Int32)
  TScientific -> Gen.realFrac_ (fromIntegral <$> Range.linearBounded @Int32)

  TUTCTime    -> do
    UTCTime 
      <$> genLiteral TDay 
      <*> do fromIntegral @Int32 <$> Gen.integral (Range.linear 0 86401)

  TDay        -> do
    Gen.just $ fromGregorianValid 
      <$> Gen.integral (Range.linear 1970 3000) 
      <*> Gen.integral (Range.linear 1 12) 
      <*> Gen.integral (Range.linear 1 31)

  TLocalTime -> do
    LocalTime <$> genLiteral TDay
              <*> genLiteral TTimeOfDay

  TTimeOfDay -> do
    Gen.just $ 
      makeTimeOfDayValid 
        <$> Gen.integral (Range.linear 0 23) 
        <*> Gen.integral (Range.linear 0 59) 
        <*> do fromInteger <$> Gen.integral (Range.linear 0 60)

  TDiffTime -> do
    fromIntegral <$> Gen.integral (Range.linearBounded @Int32)

  TNominalDiffTime -> 
    fromIntegral <$> Gen.integral (Range.linearBounded @Int32)

  TText -> Gen.text (Range.linear 0 512) (genLiteral TChar)
  TLazyText -> LazyText.fromStrict <$> genLiteral TText

  TCIText -> mk <$> genLiteral TText
  TCILazyText -> mk <$> genLiteral TLazyText

  TByteString -> StrictByteString.pack <$> Gen.list (Range.linear 0 512) (Gen.integral (Range.linearBounded @Word8))
  TLazyByteString -> LazyByteString.pack <$> Gen.list (Range.linear 0 512) (Gen.integral (Range.linearBounded @Word8))

  TUUID -> 
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
    ATypedQuery q <- forAll (genTypedQuery Empty)
    test do
      c <- liftIO getC

      res <- evalIO $ Rel8.select c $ compileQuery NoResults q
      -- diff res Set.member (evalQuery NoEvalResults q)

      return ()

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
