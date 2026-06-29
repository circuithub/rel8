{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

module Rel8.Internal.Table.Name
  ( namesFromLabels
  , namesFromLabelsTagged
  , namesFromLabelsWith
  , namesFromLabelsWithA
  , showLabels
  , showNames
  , shortenName
  )
where

-- base
import Data.Foldable ( fold )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty ( NonEmpty, intersperse, nonEmpty )
import Data.Maybe ( fromMaybe )
import Prelude

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- rel8
import Rel8.Internal.Schema.HTable (htabulateA, hfield, hspecs)
import Rel8.Internal.Schema.Name ( Name( Name ) )
import Rel8.Internal.Schema.Spec ( Spec(..) )
import Rel8.Internal.Table ( Table(..) )

-- semigroupoids
import Data.Functor.Apply (Apply)

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState)


-- | Construct a table in the 'Name' context containing the names of all
-- columns. Nested column names will be combined with @/@, the resulting
-- name will be truncated and a unique tag appended to the end of the name
-- so that the resulting name has 63 or less characters (Postgres' default
-- maximum column name length).
--
-- See also: 'namesFromLabelsTagged', 'namesFromLabelsWith'.
namesFromLabels :: Table Name a => a
namesFromLabels = namesFromLabelsWithA (shortenName Nothing) `evalState` Opaleye.start


-- | Similar to 'namesFromLabels', but receives an additional 'Opaleye.Tag'
-- to distinguish between relations. Resulting names will also have 63 or
-- less characters.
namesFromLabelsTagged :: Table Name a => Opaleye.Tag -> a
namesFromLabelsTagged relationTag = namesFromLabelsWithA (shortenName (Just relationTag)) `evalState` Opaleye.start


-- | Map a non-empty list of labels to a short SQL identifier with an opaleye tag appended,
-- truncated if it would be too large.
shortenName :: Maybe Opaleye.Tag -> NonEmpty String -> State Opaleye.Tag String
shortenName mtag labels = do
  subtag <- Opaleye.fresh
  let
    addRelationTag = case mtag of
      Nothing -> id
      Just tag -> Opaleye.tagWith tag
    suffix = addRelationTag (Opaleye.tagWith subtag "")
  pure $ take (63 - length suffix) label ++ suffix
  where
    label = fold (intersperse "/" labels)


-- | Construct a table in the 'Name' context containing the names of all
-- columns. The supplied function can be used to transform column names.
--
-- This function can be used to generically derive the columns for a
-- 'TableSchema'. For example,
--
-- @
-- myTableSchema :: TableSchema (MyTable Name)
-- myTableSchema = TableSchema
--   { columns = namesFromLabelsWith last
--   }
-- @
--
-- will construct a 'TableSchema' where each columns names exactly corresponds
-- to the name of the Haskell field.
namesFromLabelsWith :: Table Name a
  => (NonEmpty String -> String) -> a
namesFromLabelsWith = runIdentity . namesFromLabelsWithA . (pure .)


namesFromLabelsWithA :: (Apply f, Table Name a)
  => (NonEmpty String -> f String) -> f a
namesFromLabelsWithA f = fmap fromColumns $ htabulateA $ \field ->
  case hfield hspecs field of
    Spec {labels} -> Name <$> f (renderLabels labels)


showLabels :: forall a. Table (Context a) a => a -> [NonEmpty String]
showLabels _ = getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    Spec {labels} -> Const (pure (renderLabels labels))


showNames :: forall a. Table Name a => a -> NonEmpty String
showNames (toColumns -> names) = getConst $
  htabulateA @(Columns a) $ \field -> case hfield names field of
    Name name -> Const (pure name)


renderLabels :: [String] -> NonEmpty String
renderLabels labels = fromMaybe (pure "anon") (nonEmpty labels )
