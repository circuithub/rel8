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
import Rel8.Schema.Context ( Name( Name ) )
import Rel8.Schema.HTable ( htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )


namesFromLabels :: (Table a, Context a ~ Name) => a
namesFromLabels = namesFromLabelsWith go
  where
    go = fold . intersperse "/" . fmap quietSnake


namesFromLabelsWith :: (Table a, Context a ~ Name)
  => (NonEmpty String -> String) -> a
namesFromLabelsWith f = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec labels _ _ _ -> Name (f (renderLabels labels))


showLabels :: forall a. Table a => a -> [NonEmpty String]
showLabels _ = getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    SSpec labels _ _ _ -> Const [renderLabels labels]


showNames :: forall a. (Table a, Context a ~ Name) => a -> [String]
showNames (toColumns -> names) = getConst $
  htabulateA @(Columns a) $ \field -> case hfield names field of
    Name name -> Const [name]
