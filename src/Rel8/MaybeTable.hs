{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..) ) where

import Rel8.Rewrite
import Rel8.ZipLeaves


data MaybeTable t =
  MaybeTable
    { isNull :: () -- TODO! Expr IO Bool
    , maybeTable :: t
    }


instance Rewrite f g a b => Rewrite f g ( MaybeTable a ) ( MaybeTable b ) where
  rewrite f MaybeTable{..} =
    MaybeTable { isNull, maybeTable = rewrite f maybeTable }


instance ZipLeaves a b f g => ZipLeaves ( MaybeTable a ) ( MaybeTable b ) f g where
  type CanZipLeaves ( MaybeTable a ) ( MaybeTable b ) c =
    CanZipLeaves a b c

  zipLeaves proxy f a b = do
    isNull <-
      pure () -- zipLeaves proxy _ ( isNull a ) ( isNull b )

    maybeTable <-
      zipLeaves proxy f ( maybeTable a ) ( maybeTable b )

    return MaybeTable{..}
