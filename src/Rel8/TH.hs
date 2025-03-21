{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Rel8.TH (deriveRel8able) where

import Prelude ((.), pure, (<$>), ($), fail, map, id, (==), (<>), show, last, error, otherwise)
import Language.Haskell.TH (Name, Q, Dec, conT, Type (AppT, ConT, VarT, TupleT), newName, conP, varP, nameBase, conE, varE, appsE, TyVarBndr(..), varT, tupleT)
import Language.Haskell.TH.Datatype (reifyDatatype, ConstructorInfo (ConstructorInfo), DatatypeInfo (DatatypeInfo), datatypeCons, constructorFields, ConstructorVariant (RecordConstructor), constructorVariant, constructorName, datatypeVars)
import Rel8.Generic.Rel8able ( Rel8able(..) )
import Rel8.Schema.Result (Result)
import Data.Foldable (foldl', toList )
import Rel8.Schema.HTable.Identity (HIdentity(HIdentity))
import Rel8.Schema.HTable.Product (HProduct(HProduct))
import Data.Traversable (for)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Rel8.Kind.Context (SContext(..))
import Data.Functor ( (<&>) )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Rel8.Column ( Column )
import Rel8.Expr ( Expr )
import Rel8.Table ( Columns )

deriveRel8able :: Name -> Q [Dec]
deriveRel8able name = do
  DatatypeInfo{ datatypeVars = (last -> fBinder), datatypeCons = [ ConstructorInfo{ constructorName, constructorFields = f1:fs, constructorVariant = RecordConstructor (fieldName1:fieldNames) } ]} <- reifyDatatype name

  let f = case fBinder of
            PlainTV a _    -> a
            KindedTV a _ _ -> a

  contextName <- newName "context"
  name1 <- newName $ nameBase fieldName1
  names <- for fieldNames $ newName . nameBase

  let allNames = name1 :| names

  let
    unpackP =
      foldl'
        (\e n -> [p| HProduct $e (HIdentity $( varP n )) |])
        [p| HIdentity $( varP name1 ) |]
        names

    unmk (x :| xs) =
      foldl'
        (\e n -> [| HProduct $e (HIdentity $n) |])
        [| HIdentity $x |]
        xs

    mk xs = appsE (conE constructorName : toList xs)

  id
    [d| instance Rel8able $( conT name ) where
          type GColumns $( conT name) =
            $(
              foldl'
                (\t x -> [t| HProduct $t $(unColumn f x) |])
                (unColumn f f1)
                fs
            )

          type GFromExprs $( conT name ) =
            $( conT name ) Result

          gfromColumns $( varP contextName ) $unpackP =
            case $( varE contextName ) of
              SExpr      -> $( mk $ varE <$> allNames )
              SField     -> $( mk $ varE <$> allNames )
              SName      -> $( mk $ varE <$> allNames )
              SResult    -> $( mk $ allNames <&> \x -> [| runIdentity $( varE x ) |] )

          gtoColumns $(varP contextName) $( conP constructorName (map varP (name1:names)) ) =
            case $( varE contextName ) of
              SExpr      -> $( unmk $ varE <$> allNames )
              SField     -> $( unmk $ varE <$> allNames )
              SName      -> $( unmk $ varE <$> allNames )
              SResult    -> $( unmk $ allNames <&> \x -> [| Identity $( varE x ) |] )

          gfromResult $unpackP =
            $( mk $ allNames <&> \x -> [| runIdentity $( varE x ) |] )

          gtoResult $( conP constructorName (map varP (name1:names)) ) =
            $( unmk $ allNames <&> \x -> [| Identity $( varE x ) |] )
    |]


unColumn :: Name -> Type -> Q Type
unColumn _ (AppT (AppT (ConT _Column) _f) t) | _Column == ''Column = [t| HIdentity $(pure t) |]
unColumn f t = [t| Columns $(instantiate t) |]
  where
    instantiate = \case
      VarT v | v == f    -> [t| Expr |]
             | otherwise -> varT v

      AppT x y -> [t| $(instantiate x) $(instantiate y) |]

      TupleT n -> tupleT n

      ConT n -> conT n

      other -> error $ show other
