{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Rel8.TH
  ( deriveRel8able
  , deriveRel8ables
  ) where

import Control.Monad (zipWithM)
import Data.Foldable (toList)
import Data.Foldable1 (foldr1)
import Data.List (unsnoc)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (type (==))
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype (ConstructorVariant (RecordConstructor), DatatypeInfo (..), constructorFields, constructorVariant, datatypeCons, reifyDatatype)
import qualified Language.Haskell.TH.Datatype as TH.Datatype
import qualified Language.Haskell.TH.Syntax as TH
import Rel8.Internal.Column (Column)
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Generic.Rel8able (Rel8able (..), Serialize, deserialize, serialize)
import Rel8.Internal.Kind.Context (SContext (..))
import Rel8.Internal.Schema.HTable.Identity (HIdentity)
import Rel8.Internal.Schema.HTable.Label (HLabel (..))
import Rel8.Internal.Schema.HTable.Product (HProduct (HProduct))
import Rel8.Internal.Schema.Kind (Context)
import Rel8.Internal.Schema.Result (Result)
import Rel8.Internal.Table (Columns, Transpose, fromColumns, toColumns)
import Rel8.Internal.Table.Serialize (ToExprs)
import Prelude hiding (foldr1)

-- | Represent a valid datatype
data ParsedDatatype
    = ParsedDatatype
    { name :: TH.Name
    , conName :: TH.Name
    , fBinder :: TH.Name
    , fields :: NonEmpty ParsedField
    }
    deriving (Show)

-- | Represent a valid field
data ParsedField
    = ParsedField
    { fieldSelector :: Maybe TH.Name
    , fieldType :: TH.Type
    , fieldColumnType :: TH.Type
    , fieldFreshName :: TH.Name
    }
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
    fieldList <- zipWithM (parseField fBinder) (constructorFields constructor) fieldSelectors
    fields <- maybe (prettyFail "Expected at least one field") pure $ nonEmpty fieldList
    pure ParsedDatatype{..}

parseFBinder :: TH.Type -> Q TH.Name
parseFBinder (TH.SigT x (TH.ConT kind))
    | kind == ''Context = parseFBinder x
    | otherwise = prettyFail $ "expected kind encountered for the context type argument: " ++ show kind
-- type Context = Type -> Type
parseFBinder (TH.SigT x (TH.ArrowT `TH.AppT` TH.StarT `TH.AppT` TH.StarT)) = parseFBinder x
parseFBinder (TH.VarT name) = pure name
parseFBinder typ = prettyFail $ "unexpected type encountered while looking for the context type argument to the datatype: " ++ show typ

parseField :: TH.Name -> TH.Type -> Maybe TH.Name -> Q ParsedField
parseField fBinder fieldType fieldSelector = do
    n <- TH.newName "x"
    let ft = TH.Datatype.applySubstitution (M.fromList [(fBinder, TH.ConT ''Expr)]) $ resolveColumnF fBinder fieldType
    columnType <- case ft of
        -- Without special casing this, we get lots of UndecidableInstance errors
        -- ie, rewrite Expr \phi to HIdentity \phi
        (TH.ConT exprName' `TH.AppT` x) | exprName' == ''Expr -> [t|HIdentity $(pure x)|] --
        _ -> [t|Columns $(pure ft)|]
    pure $ ParsedField{fieldSelector = fieldSelector, fieldType = ft, fieldColumnType = columnType, fieldFreshName = n}

-- | Like foldr1, but we create a mostly balanced binary tree.
-- This makes a big difference for compile times, since we want the depth of the HProduct tree to be minimal.
-- Each layer adds a lot of overhead.
foldr1Tree :: (a -> a -> a ) -> NonEmpty a -> a
foldr1Tree f xs0 = go (toList xs0) size0
  where
    size0 = length xs0
    go [] _ = error "impossible"
    go [x] _ = x
    go [x,y] _ = f x y
    go xs size = f (go left half) (go right (size - half))
      where
        -- Invariants:
        -- half > 0, since size is >2, this will always be the case
        -- size - half > 0
        half = size `div` 2
        (left, right) = splitAt half xs

generateGColumns :: ParsedDatatype -> Q TH.Type
generateGColumns ParsedDatatype{..} =
    foldr1Tree (\x y -> [t|HProduct $x $y|]) $ fmap generateGColumn fields
  where
    generateGColumn ParsedField{..} =
        labelled fieldSelector [t|$(pure fieldColumnType)|]
    labelled Nothing x = x
    labelled (Just (TH.Name (TH.OccName fieldSelector) _)) x = [t|HLabel $(TH.litT $ TH.strTyLit fieldSelector) $x|]

-- | Generate an expression to construct a column value
generateColumnsE :: ParsedDatatype -> (Q TH.Type -> Q TH.Exp -> Q TH.Exp) -> Q TH.Exp
generateColumnsE ParsedDatatype{..} g =
    foldr1Tree (\x y -> TH.conE 'HProduct `TH.appE` x `TH.appE` y) $ fmap generateColumnE fields
  where
    generateColumnE ParsedField{..} =
        labelled fieldSelector $
            g (pure fieldType) $
                TH.varE fieldFreshName
    labelled Nothing x = x
    labelled (Just _) x = TH.conE 'HLabel `TH.appE` x

-- | Generate a pattern to destruct a column
generateColumnsP :: ParsedDatatype -> TH.Pat
generateColumnsP ParsedDatatype{..} =
    foldr1Tree (\x y -> TH.ConP 'HProduct [] [x, y]) $ fmap generateColumnP fields
  where
    generateColumnP ParsedField{..} =
        labelled fieldSelector $
            TH.VarP fieldFreshName
    labelled Nothing x = x
    labelled (Just _) x = TH.ConP 'HLabel [] [x]

-- | Generate an expression to create the constructor
generateConstructorE :: ParsedDatatype -> (Q TH.Type -> Q TH.Exp -> Q TH.Exp) -> Q TH.Exp
generateConstructorE parsedDatatype g =
    foldl' TH.appE (TH.conE (conName parsedDatatype)) . fmap generateFieldE $ fields parsedDatatype
  where
    generateFieldE ParsedField{..} =
        g (pure fieldType) $ TH.varE fieldFreshName

-- | Generate a pattern to destruct the datatype
generateConstructorP :: ParsedDatatype -> Q TH.Pat
generateConstructorP parsedDatatype =
  pure $ TH.ConP (conName parsedDatatype) [] . toList . fmap (TH.VarP . fieldFreshName) $ fields parsedDatatype


-- These two functions exist solely so we can write the splices without using TypeApplications, which require an extra language extension in client code, and are required here to appease the type checker.
-- Otherwise it gets confused.
deserialize' :: forall transposition expr a. Proxy expr -> (Serialize transposition expr a, transposition ~ (a == Transpose Result expr)) => Columns expr Result -> a
deserialize' _ = deserialize @_ @expr

serialize' :: forall transposition expr a. Proxy expr -> (Serialize transposition expr a, transposition ~ (a == Transpose Result expr)) => a -> Columns expr Result
serialize' _ = serialize @_ @expr

-- | Derive a 'Rel8able' instance using TemplateHaskell.
-- Using TH can be signficantly faster than using Generics.
-- Currently, this doesn't support all of the features of the Generics deriving machinery.
--
-- You might have to enable @UndecidableInstances@ for instances to compile.
--
-- >>> data Foo f  = Foo
-- >>>   { fooId :: Column f Word64
-- >>>   , fooName :: Column f Text
-- >>>   }
-- >>>
-- >>>  deriveRel8able ''Foo
deriveRel8able :: TH.Name -> Q [TH.Dec]
deriveRel8able name = do
    datatypeInfo <- reifyDatatype name
    parsedDatatype <- parseDatatype datatypeInfo
    let gColumns = generateGColumns parsedDatatype
    let constructorE = generateConstructorE parsedDatatype
    let constructorP = generateConstructorP parsedDatatype
    let columnsE = generateColumnsE parsedDatatype
    let columnsP = pure $ generateColumnsP parsedDatatype
    contextName <- TH.newName "context"
    [d|
        -- We already derive ToExprs for Rel8able instances but we assumed they are Generically derived.
        -- So, we need to allow this one to overlap, but this is fine since this instance is always more specific.
        instance {-# OVERLAPPING #-} (x ~ $(TH.conT name) Expr, result ~ Result) => ToExprs x ($(TH.conT name) result)

        instance Rel8able $(TH.conT name) where
            -- Really the Generic code substitutes Expr for f and then does stuff. Maybe we want to move closer to that?
            type
                GColumns $(TH.conT name) =
                    $gColumns

            type
                GFromExprs $(TH.conT name) =
                    $(TH.conT name) Result

            -- the rest of the definition is just a few functions to go back and forth between Columns and the datatype
            gfromColumns $(TH.varP contextName) v =
                case $(TH.varE contextName) of
                    SResult -> case v of $columnsP -> $(constructorE (\ft x -> [|deserialize' (Proxy :: Proxy $ft) $x|]))
                    SExpr -> case v of $columnsP -> $(constructorE (\_ x -> [|fromColumns $x|]))
                    SField -> case v of $columnsP -> $(constructorE (\_ x -> [|fromColumns $x|]))
                    SName -> case v of $columnsP -> $(constructorE (\_ x -> [|fromColumns $x|]))

            gtoColumns $(TH.varP contextName) $constructorP =
                case $(TH.varE contextName) of
                    SExpr -> $(columnsE (\_ x -> [|toColumns $x|]))
                    SField -> $(columnsE (\_ x -> [|toColumns $x|]))
                    SName -> $(columnsE (\_ x -> [|toColumns $x|]))
                    SResult -> $(columnsE (\ft x -> [|serialize' (Proxy :: Proxy $ft) $x|]))

            gfromResult $columnsP =
                $(constructorE (\ft x -> [|deserialize' (Proxy :: Proxy $ft) $x|]))

            gtoResult $constructorP =
                $(columnsE (\ft x -> [|serialize' (Proxy :: Proxy $ft) $x|]))
        |]

-- | Like 'deriveRel8able' but for a list of datatypes.
-- This is helpful as all of the instances live in a single splice.
-- Each TH splice creates a new decleration group, so they cannot see instances later in the file.
-- By deriving the instances in the same splice, we can ensure that they see each other.
-- This is necessary when deriving cyclic instances, but also reduces the amount of splice sorting required.
-- There is also a small performance overhead to each TH splice.
deriveRel8ables :: [TH.Name] -> Q [TH.Dec]
deriveRel8ables xs = concat <$> traverse deriveRel8able xs

-- | Walk 'TH.Type' and replace all occurences of @Column f x@ with @Expr x@.
resolveColumnF :: TH.Name -> TH.Type -> TH.Type
resolveColumnF fBinder (TH.ForallT tvs context t) =
    TH.ForallT tvs context (resolveColumnF fBinder t)
resolveColumnF fBinder (TH.AppT f x)
    | TH.ConT columnName `TH.AppT` (TH.VarT fBinder') <- f
    , columnName == ''Column
    , fBinder == fBinder' =
        TH.AppT (TH.ConT ''Expr) (resolveColumnF fBinder x)
    | otherwise = TH.AppT (resolveColumnF fBinder f) (resolveColumnF fBinder x)
resolveColumnF fBinder (TH.SigT t k) = TH.SigT (resolveColumnF fBinder t) (resolveColumnF fBinder k) -- k could be Kind
resolveColumnF fBinder (TH.InfixT l c r) = TH.InfixT (resolveColumnF fBinder l) c (resolveColumnF fBinder r)
resolveColumnF fBinder (TH.UInfixT l c r) = TH.UInfixT (resolveColumnF fBinder l) c (resolveColumnF fBinder r)
resolveColumnF fBinder (TH.ParensT t) = TH.ParensT (resolveColumnF fBinder t)
#if MIN_VERSION_template_haskell(2,15,0)
resolveColumnF fBinder (TH.AppKindT t k)  = TH.AppKindT (resolveColumnF fBinder t) (resolveColumnF fBinder k)
resolveColumnF fBinder (TH.ImplicitParamT n t)
  = TH.ImplicitParamT n (resolveColumnF fBinder t)
#endif
#if MIN_VERSION_template_haskell(2,16,0)
resolveColumnF fBinder (TH.ForallVisT tvs t) =
  TH.ForallVisT tvs (resolveColumnF fBinder t)
#endif
#if MIN_VERSION_template_haskell(2,19,0)
resolveColumnF fBinder (TH.PromotedInfixT l c r)
  = TH.PromotedInfixT (resolveColumnF fBinder l) c (resolveColumnF fBinder r)
resolveColumnF fBinder (TH.PromotedUInfixT l c r)
  = TH.PromotedUInfixT (resolveColumnF fBinder l) c (resolveColumnF fBinder r)
#endif
resolveColumnF _ t = t
