{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Returning
  ( Returning(..), ppReturning
  )
where

-- base
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.Int ( Int64 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), text )

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table.Opaleye ( castTable, exprs, view )
import Rel8.Table.Serialize ( Serializable )


-- | @INSERT@, @UPDATE@ and @DELETE@ all support returning either the number of
-- rows affected, or the actual rows modified. 'Projection' allows you to
-- project out of these returned rows, which can be useful if you want to log
-- exactly which rows were deleted, or to view a generated id (for example, if
-- using a column with an autoincrementing counter as a default value).
type Returning :: Type -> Type -> Type
data Returning names a where
  NumberOfRowsAffected :: Returning names Int64
  Projection :: (Selects names exprs, Serializable projection a)
    => (exprs -> projection)
    -> Returning names [a]


ppReturning :: TableSchema names -> Returning names a -> Doc
ppReturning TableSchema {columns} = \case
  NumberOfRowsAffected -> mempty
  Projection projection ->
    text "RETURNING" <+> Opaleye.commaV Opaleye.ppSqlExpr (toList sqlExprs)
    where
      sqlExprs =
        Opaleye.sqlExpr <$> exprs (castTable (projection (view columns)))
