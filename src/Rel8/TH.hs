{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Rel8.TH (deriveRel8able) where

import Prelude (show, (.), pure, (<$>), Maybe (Nothing), ($), fail)
import Language.Haskell.TH (Name, Q, Dec, instanceD, cxt, appT, conT, funD, clause, normalB, tySynEqn, tySynInstD, reportWarning, Type (AppT), newName, conP, varP, mkName, nameBase)
import Language.Haskell.TH.Datatype (reifyDatatype, ConstructorInfo (ConstructorInfo), DatatypeInfo (DatatypeInfo), datatypeCons, constructorFields, ConstructorVariant (RecordConstructor), constructorVariant)
import Rel8.Generic.Rel8able ( Rel8able(..) )
import Rel8.Schema.Result (Result)
import Data.Foldable (for_, foldr)
import Rel8.Schema.HTable.Identity (HIdentity)
import Rel8.Schema.HTable.Product (HProduct)
import Language.Haskell.TH.Syntax (showName)
import Data.Traversable (for)

deriveRel8able :: Name -> Q [Dec]
deriveRel8able name = do
  DatatypeInfo{ datatypeCons = [ ConstructorInfo{ constructorFields = f1:fs, constructorVariant = RecordConstructor (fieldName1:fieldNames) } ]} <- reifyDatatype name

  for_ (f1:fs) (reportWarning . show)

  pure <$> instanceD (cxt []) (appT (conT ''Rel8able) (conT name))
    [ tySynInstD $ tySynEqn Nothing (appT (conT ''GColumns) (conT name)) $
        foldr
          (appT . appT (conT ''HProduct) . appT (conT ''HIdentity) . unColumn)
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
        clause [varP name1] (normalB [| undefined |]) []

    , funD 'gtoResult $ pure $
        clause [] (normalB [|undefined|]) []
    ]


unColumn :: Type -> Q Type
unColumn (AppT (AppT _Column _f) t) = pure t
unColumn _ = fail "Not a 'Column f' application"