{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Name
  ( namesFromLabels
  , namesFromLabelsWith
  , showExprs
  , showLabels
  , showNames
  )
where

-- base
import Data.Foldable ( fold )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.List.NonEmpty ( NonEmpty, intersperse )
import Prelude

-- casing
import Text.Casing ( quietSnake )

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Kind.Labels ( renderLabels )
import Rel8.Schema.HTable ( htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.Name ( Name, Col(..) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )


namesFromLabels :: Table Name a => a
namesFromLabels = namesFromLabelsWith go
  where
    go = fold . intersperse "/" . fmap quietSnake


namesFromLabelsWith :: Table Name a
  => (NonEmpty String -> String) -> a
namesFromLabelsWith f = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {labels} -> NameCol (f (renderLabels labels))


showExprs :: Table Expr a => a -> [(String, String)]
showExprs as = case (namesFromLabels, toColumns as) of
  (names, exprs) -> getConst $ htabulateA $ \field ->
    case (hfield names field, hfield exprs field) of
      (NameCol name, DB expr) -> Const [(name, show expr)]


showLabels :: forall a. Table (Context a) a => a -> [NonEmpty String]
showLabels _ = getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {labels} -> Const [renderLabels labels]


showNames :: forall a. Table Name a => a -> [String]
showNames (toColumns -> names) = getConst $
  htabulateA @(Columns a) $ \field -> case hfield names field of
    NameCol name -> Const [name]
