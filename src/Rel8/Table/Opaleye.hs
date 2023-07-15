{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

{-# options_ghc -Wno-deprecations #-}

module Rel8.Table.Opaleye
  ( attributes
  , binaryspec
  , distinctspec
  , exprs
  , exprsWithNames
  , ifPP
  , relExprPP
  , table
  , tableFields
  , unpackspec
  , valuesspec
  , view
  , castTable
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Adaptors as Opaleye
import qualified Opaleye.Field as Opaleye ( Field_ )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Operators as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Table as Opaleye

-- profunctors
import Data.Profunctor ( dimap, lmap )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye
  ( fromPrimExpr, toPrimExpr
  , scastExpr, traverseFieldP
  )
import Rel8.Schema.HTable ( htabulateA, hfield, hspecs, htabulate,
                            htraverseP, htraversePWithField )
import Rel8.Schema.Name ( Name( Name ), Selects, ppColumn )
import Rel8.Schema.QualifiedName (QualifiedName (QualifiedName))
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Type.Information (showTypeName)

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )
import Data.Profunctor.Product ( ProductProfunctor )


attributes :: Selects names exprs => TableSchema names -> exprs
attributes schema@TableSchema {columns} = fromColumns $ htabulate $ \field ->
  case hfield (toColumns columns) field of
    Name column -> fromPrimExpr $ Opaleye.ConstExpr $
      Opaleye.OtherLit $
        show (ppTable schema) <> "." <> show (ppColumn column)


fromOpaleyespec :: (ProductProfunctor p, Table Expr a)
  => p (Opaleye.Field_ n x) (Opaleye.Field_ n x)
  -> p a a
fromOpaleyespec x =
  dimap toColumns fromColumns (htraverseP (traverseFieldP x))


binaryspec :: Table Expr a => Opaleye.Binaryspec a a
binaryspec = fromOpaleyespec Opaleye.binaryspecField


distinctspec :: Table Expr a => Opaleye.Distinctspec a a
distinctspec = fromOpaleyespec Opaleye.distinctspecField


exprs :: Table Expr a => a -> NonEmpty Opaleye.PrimExpr
exprs (toColumns -> as) = getConst $ htabulateA $ \field ->
  case hfield as field of
    expr -> Const (pure (toPrimExpr expr))


exprsWithNames :: Selects names exprs
  => names -> exprs -> NonEmpty (String, Opaleye.PrimExpr)
exprsWithNames names as = getConst $ htabulateA $ \field ->
  case (hfield (toColumns names) field, hfield (toColumns as) field) of
    (Name name, expr) -> Const (pure (name, toPrimExpr expr))


ifPP :: Table Expr a => Opaleye.IfPP a a
ifPP = fromOpaleyespec Opaleye.ifPPField


relExprPP :: Table Expr a => Opaleye.RelExprPP a a
relExprPP = fromOpaleyespec Opaleye.relExprColumn


table :: Selects names exprs => TableSchema names -> Opaleye.Table exprs exprs
table (TableSchema (QualifiedName name schema) columns) =
  case schema of
    Nothing -> Opaleye.table name (tableFields columns)
    Just schemaName -> Opaleye.tableWithSchema schemaName name (tableFields columns)


tableFields :: Selects names exprs
  => names -> Opaleye.TableFields exprs exprs
tableFields (toColumns -> names) = dimap toColumns fromColumns $
  unwrapApplicative $ htabulateA $ \field -> WrapApplicative $
    case hfield names field of
      name -> lmap (`hfield` field) (go name)
  where
    go :: Name a -> Opaleye.TableFields (Expr a) (Expr a)
    go (Name name) =
      traverseFieldP $
        Opaleye.requiredTableField name


unpackspec :: Table Expr a => Opaleye.Unpackspec a a
unpackspec = fromOpaleyespec Opaleye.unpackspecField
{-# INLINABLE unpackspec #-}


valuesspec :: Table Expr a => Opaleye.Valuesspec a a
valuesspec = dimap toColumns fromColumns $
  htraversePWithField (traverseFieldP . Opaleye.valuesspecFieldType . typeName)
  where
    typeName = showTypeName . info . hfield hspecs


view :: Selects names exprs => names -> exprs
view columns = fromColumns $ htabulate $ \field ->
  case hfield (toColumns columns) field of
    Name column -> fromPrimExpr $ Opaleye.BaseTableAttrExpr column


-- | Transform a table by adding 'CAST' to all columns. This is most useful for
-- finalising a SELECT or RETURNING statement, guaranteed that the output
-- matches what is encoded in each columns TypeInformation.
castTable :: Table Expr a => a -> a
castTable (toColumns -> as) = fromColumns $ htabulate \field ->
  case hfield hspecs field of
    Spec {info} -> case hfield as field of
        expr -> scastExpr info expr
