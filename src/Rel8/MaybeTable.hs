{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..) ) where

import Rel8.Column


data MaybeTable t f =
  MaybeTable
    { isNull :: Column f Bool
    , maybeTable :: t
    }
