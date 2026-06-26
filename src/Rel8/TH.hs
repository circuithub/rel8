{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Rel8.TH (deriveRel8able, parseDatatype) where

import Prelude
import Rel8.Table.Serialize ( ToExprs )
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Datatype (reifyDatatype, DatatypeInfo (..), datatypeCons, constructorFields, ConstructorVariant (RecordConstructor), constructorVariant)
import qualified Language.Haskell.TH.Datatype as TH.Datatype
import Rel8.Generic.Rel8able ( Rel8able(..), Serialize, serialize, deserialize)
import Rel8.Schema.Result (Result)
import Rel8.Schema.HTable.Identity (HIdentity(HIdentity))
import Rel8.Schema.HTable.Product (HProduct(HProduct))
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Rel8.Kind.Context (SContext(..))
import Rel8.Column ( Column )
import Rel8.Expr ( Expr )
import Rel8.Table (Columns, toColumns, fromColumns, Transpose)
import Rel8.Schema.Kind (Context)
import Data.List (unsnoc)
import Rel8.Schema.HTable.Label (HLabel(..))
import Data.Proxy (Proxy(Proxy))
import qualified Data.Map.Strict as M
import Data.Type.Equality (type (==))


-- We derive a Rel8able instance using TH.
-- At it's core a Rel8able instance is a bijection between a datatype and the the SQL columns corresponding to its fields.
-- We only support datatypes with one constructor.
-- The datatype must have exactly one type arg and it is the index for our HKD stuff.
-- Question: Can we support multiple type args?
---
-- We have three types of fields:
-- 1) Column f Text : Directly using Column, easy. This is just a special case of (3)
-- 2) OtherType f : They embed another Rel8able type
-- 3) TabledType : They embed a type with a table instance.
--     eg, we might see something like (Column f Text, Column f Bool). (,) has a Table instance,
--     so we know how to map this type to SQL columns.
--
-- We represent a vector of SQL columns with basically:
-- HLabel "field label" (HIdentity Text) `HProduct` HLabel "another field" (HIdentity Bool) ...
-- Nothing too complicated here. I'm not sure if we are allowed to leave the HLabels out or if that will cause everything to explode.
-- This H* stuff is also used to thread around contexts if you look at the definitions of these things

data ParsedDatatype =
  ParsedDatatype
  { name :: TH.Name
  , conName :: TH.Name
  , fBinder :: TH.Name
  , fields :: [ParsedField]
  }
  deriving (Show)

data ParsedField =
  ParsedField
  { fieldSelector :: Maybe TH.Name
  , fieldVariant :: ParsedFieldVariant
  , fieldType :: TH.Type
  , fieldColumnType :: TH.Type
  , fieldFreshName :: TH.Name
  }
  deriving (Show)

data ParsedFieldVariant =
  ColumnField
  | TableField -- TODO rename to table field
  deriving (Show)

-- | 'fail' but indicate that the failure is coming from our code
prettyFail :: String -> Q a
prettyFail str = fail $ "deriveRel8able: " ++ str

parseDatatype :: DatatypeInfo -> Q ParsedDatatype
parseDatatype datatypeInfo = do
  constructor <-
    -- Check that it only has one constructor
    case datatypeCons datatypeInfo of
      [cons] -> pure cons
      _ -> prettyFail "exepecting a datatype with exactly 1 constructor"
  let conName = TH.Datatype.constructorName constructor
  let name = datatypeName datatypeInfo
  fBinder <- case unsnoc $ datatypeInstTypes datatypeInfo of
    Just (_, candidate) -> parseFBinder candidate
    Nothing -> prettyFail "expecting the datatype to have a context type parameter like `data Foo f = ...`"
  let fieldSelectors = case constructorVariant constructor of
        -- Only record constructors have field names
        RecordConstructor names -> map Just names
        _ -> repeat Nothing
  let columnName = ''Column
  fields <-
    mapM (uncurry $ parseField columnName fBinder) $
      zip (constructorFields constructor) fieldSelectors
  -- TODO: check that we have at least one field, fail otherwise
  pure ParsedDatatype{..}

parseFBinder :: TH.Type -> Q TH.Name
parseFBinder (TH.SigT x (TH.ConT kind))
  | kind == ''Context = parseFBinder x
  | otherwise = prettyFail $ "expected kind encountered for the context type argument: " ++ show kind
parseFBinder (TH.VarT name) = pure name
parseFBinder typ = prettyFail $ "unexpected type encountered while looking for the context type argument to the datatype: " ++ show typ

typeApps :: TH.Type -> [TH.Type]
typeApps x = go x []
 where
   go (TH.AppT x y) args = go x (y:args)
   go x args = x:args

unTypeApps :: TH.Type -> [TH.Type] -> TH.Type
unTypeApps = foldl' TH.AppT

-- TODO: Replace 'Column f a' with a, to avoid UndecidableInstances when using Table fields
parseField :: TH.Name -> TH.Name -> TH.Type -> Maybe TH.Name -> Q ParsedField
parseField columnName fBinder fieldType fieldSelector
  | (TH.ConT columnCandidate `TH.AppT` TH.VarT fBinderCandidate `TH.AppT` subType) <- fieldType
  , columnCandidate == columnName
  , fBinderCandidate == fBinder
     = do
        n <- TH.newName "x"
        pure $ ParsedField { fieldSelector = fieldSelector, fieldVariant =  ColumnField, fieldType = subType, fieldColumnType = TH.ConT ''HIdentity `TH.AppT` subType, fieldFreshName = n}
  | otherwise
     = do
        n <- TH.newName "x"
        columnType <- [t|Columns ($(pure $ TH.Datatype.applySubstitution (M.fromList [(fBinder, TH.ConT ''Expr)]) $ fieldType)) |]
        ft2 <- [t|($(pure $ TH.Datatype.applySubstitution (M.fromList [(fBinder, TH.ConT ''Expr)]) $ fieldType)) |]
        pure $ ParsedField { fieldSelector = fieldSelector, fieldVariant =  TableField, fieldType = ft2, fieldColumnType = columnType, fieldFreshName = n}
  | otherwise = prettyFail $ "Field of unexpected type: "  ++ show fieldType ++ show (typeApps fieldType)

generateGColumns :: ParsedDatatype -> Q TH.Type
generateGColumns ParsedDatatype{..} =
  foldr1 (\x y -> [t|HProduct $x $y|]) $ map generateGColumn fields
  where
    generateGColumn ParsedField{..} =
      [t| $(pure fieldColumnType)|]
      >>= labelled fieldSelector
    labelled Nothing x = pure x
    labelled (Just (TH.Name (TH.OccName fieldSelector) _)) x = [t|HLabel $(TH.litT $ TH.strTyLit fieldSelector) $(pure x)|]

generateColumnsE :: ParsedDatatype -> (Q TH.Exp -> Q TH.Exp) -> (Q TH.Type -> Q TH.Exp -> Q TH.Exp) -> Q TH.Exp
generateColumnsE ParsedDatatype{..} f g =
  foldr1 (\x y -> TH.conE 'HProduct `TH.appE` x `TH.appE` y) $ map generateColumnE fields
  where
    generateColumnE ParsedField{..} =
      labelled fieldSelector $
      case fieldVariant of
        ColumnField -> TH.conE 'HIdentity `TH.appE` f (TH.varE fieldFreshName)
        TableField -> g (pure fieldType) $ TH.varE fieldFreshName
    labelled Nothing x = x
    labelled (Just _) x = TH.conE 'HLabel `TH.appE`x

generateColumnsP :: ParsedDatatype -> TH.Pat
generateColumnsP ParsedDatatype{..} =
  foldr1 (\x y -> TH.ConP 'HProduct [] [x, y]) $ map generateColumnP fields
  where
    generateColumnP ParsedField{..} =
      labelled fieldSelector $
      case fieldVariant of
        ColumnField ->  TH.ConP 'HIdentity [] [TH.VarP fieldFreshName]
        TableField -> TH.VarP fieldFreshName
    labelled Nothing x = x
    labelled (Just _) x = TH.ConP 'HLabel [] [x]

generateConstructorE :: ParsedDatatype -> (Q TH.Exp -> Q TH.Exp) -> (Q TH.Type -> Q TH.Exp -> Q TH.Exp) -> Q TH.Exp
generateConstructorE parsedDatatype f g =
  foldl' TH.appE (TH.conE (conName parsedDatatype)) . map generateFieldE $ fields parsedDatatype
  where
    generateFieldE ParsedField{..} =
      case fieldVariant of
        ColumnField -> f . TH.varE $ fieldFreshName
        TableField -> g (pure fieldType) $ TH.varE fieldFreshName

-- These two functions exist solely so we can write the splices without using TypeApplications, which require an extra language extension in client code, and are required here to appease the type checker.
-- Otherwise it gets confused.
deserialize' :: forall transposition expr a. Proxy expr -> (Serialize transposition expr a, transposition ~ (a == Transpose Result expr)) => Columns expr Result -> a
deserialize' _ = deserialize @_ @expr

serialize' :: forall transposition expr a. Proxy expr -> (Serialize transposition expr a, transposition ~ (a == Transpose Result expr)) => a -> Columns expr Result
serialize' _ = serialize @_ @expr

deriveRel8able :: TH.Name -> Q [TH.Dec]
deriveRel8able name = do
  datatypeInfo <- reifyDatatype name
  parsedDatatype <- parseDatatype datatypeInfo
  let gColumns = generateGColumns parsedDatatype
  let constructorE = generateConstructorE parsedDatatype
  let constructorP = pure $ TH.ConP (conName parsedDatatype) [] . map (TH.VarP . fieldFreshName) $ fields parsedDatatype
  let columnsE = generateColumnsE parsedDatatype 
  let columnsP = pure $ generateColumnsP parsedDatatype
  contextName <- TH.newName "context"
  [d|
   instance {-# OVERLAPPING #-} (x ~ $(TH.conT name) Expr, result ~ Result) => ToExprs x ($(TH.conT name) result)
   instance Rel8able $(TH.conT name) where
          -- Really the Generic code substitutes Expr for f and then does stuff. Maybe we want to move closer to that?
          type GColumns $( TH.conT name) =
            $gColumns

          type GFromExprs $( TH.conT name ) =
            $( TH.conT name ) Result

          -- the rest of the definition is just a few functions to go back and forth between Columns and the datatype
          gfromColumns $( TH.varP contextName ) x =
            case $( TH.varE contextName ) of
              SResult    -> case x of $columnsP -> $(constructorE (\x -> [| runIdentity $x |] ) (\ft x -> [| deserialize' (Proxy :: Proxy $ft) $x |]))
              SExpr      -> case x of $columnsP -> $(constructorE id (\_ x -> [| fromColumns $x |] ))
              SField     -> case x of $columnsP -> $(constructorE id (\_ x -> [| fromColumns $x |] ))
              SName      -> case x of $columnsP -> $(constructorE id (\_ x -> [| fromColumns $x |] ))

          gtoColumns $(TH.varP contextName) $constructorP =
            case $( TH.varE contextName ) of
              SExpr      -> $(columnsE id (\_ x -> [| toColumns $x |]))
              SField     -> $(columnsE id (\_ x -> [| toColumns $x |]))
              SName      -> $(columnsE id (\_ x -> [| toColumns $x |]))
              SResult    -> $(columnsE (\x -> [| Identity $x |] ) (\ft x -> [| serialize' (Proxy :: Proxy $ft) $x |]))

          gfromResult $columnsP = 
              $( constructorE (\x -> [|runIdentity $x |]) (\ft x -> [| deserialize' (Proxy :: Proxy $ft) $x |] ))

          gtoResult $constructorP =
              $( columnsE (\x -> [| Identity $x |])  (\ft x -> [| serialize' (Proxy :: Proxy $ft) $x |] ))

   |]
