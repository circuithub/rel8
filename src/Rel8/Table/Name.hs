{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Rel8.Table.Name
  ( namesFromLabels
  , namesFromLabelsWith
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
import Rel8.Kind.Labels ( renderLabels )
import Rel8.Schema.Context ( Col'(..), Name( Name ) )
import Rel8.Schema.HTable ( htabulate, htabulateA, hfield, hspecs )
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
    SSpec {labels} -> Col (Name (f (renderLabels labels)))


showLabels :: forall a. Table (Context a) a => a -> [NonEmpty String]
showLabels _ = getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec {labels} -> Const [renderLabels labels]


showNames :: forall a. Table Name a => a -> [String]
showNames (toColumns -> names) = getConst $
  htabulateA @(Columns a) $ \field -> case hfield names field of
    Col (Name name) -> Const [name]
