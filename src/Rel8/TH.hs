{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Rel8.TH (deriveRel8able) where

import Prelude ((.), pure, (<$>), Maybe (Nothing), ($), fail, map)
import Language.Haskell.TH (Name, Q, Dec, instanceD, cxt, appT, conT, funD, clause, normalB, tySynEqn, tySynInstD, Type (AppT), newName, conP, varP, nameBase, conE, appE, varE, appsE)
import Language.Haskell.TH.Datatype (reifyDatatype, ConstructorInfo (ConstructorInfo), DatatypeInfo (DatatypeInfo), datatypeCons, constructorFields, ConstructorVariant (RecordConstructor), constructorVariant, constructorName)
import Rel8.Generic.Rel8able ( Rel8able(..) )
import Rel8.Schema.Result (Result)
import Data.Foldable (foldl')
import Rel8.Schema.HTable.Identity (HIdentity(HIdentity))
import Rel8.Schema.HTable.Product (HProduct(HProduct))
import Data.Traversable (for)
import Data.Functor.Identity (Identity(Identity))

deriveRel8able :: Name -> Q [Dec]
deriveRel8able name = do
  DatatypeInfo{ datatypeCons = [ ConstructorInfo{ constructorName, constructorFields = f1:fs, constructorVariant = RecordConstructor (fieldName1:fieldNames) } ]} <- reifyDatatype name

  pure <$> instanceD (cxt []) (appT (conT ''Rel8able) (conT name))
    [ tySynInstD $ tySynEqn Nothing (appT (conT ''GColumns) (conT name)) $
        foldl'
          (\e x -> appT (appT (conT ''HProduct) e) (appT (conT ''HIdentity) (unColumn x)))
          (appT (conT ''HIdentity) (unColumn f1))
          fs

    , tySynInstD $ tySynEqn Nothing (appT (conT ''GFromExprs) (conT name)) $
        appT (conT name) (conT ''Result)

    , funD 'gfromColumns $ pure $
        clause [] (normalB [|undefined|]) []

    , funD 'gtoColumns $ pure $
        clause [] (normalB [|undefined|]) []

    , funD 'gfromResult $ pure do
        name1 <- newName $ nameBase fieldName1
        names <- for fieldNames $ newName . nameBase
        clause
          [foldl' (\pat n -> conP 'HProduct [pat, conP 'HIdentity [conP 'Identity [varP n]]])
                  (conP 'HIdentity [conP 'Identity [varP name1]]) 
                  names]
          (normalB (appsE (conE constructorName : (varE <$> (name1:names)))))
          []

    , funD 'gtoResult $ pure do
        name1 <- newName $ nameBase fieldName1
        names <- for fieldNames $ newName . nameBase
        clause 
          [conP constructorName (map varP (name1:names))] 
          (normalB $
           foldl' (\e n -> appE (appE (conE 'HProduct) e) (appE (conE 'HIdentity) (appE (conE 'Identity) (varE n))))
                  (appE (conE 'HIdentity) (appE (conE 'Identity) (varE name1))) 
                  names
          ) []
    ]


unColumn :: Type -> Q Type
unColumn (AppT (AppT _Column _f) t) = pure t
unColumn _ = fail "Not a 'Column f' application"