{-# language LambdaCase #-}

module Rel8.Type.Builder.Fold (
  interfoldMap
) where

-- base
import Prelude


interfoldMap :: (Foldable t, Monoid m) => m -> (a -> m) -> t a -> m
interfoldMap sep f = maybe mempty id . foldr go Nothing
  where
    go x = \case
      Nothing -> Just (f x)
      Just acc -> Just (f x <> sep <> acc)
