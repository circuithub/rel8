{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options -fplugin=RecordDotPreprocessor #-}

module Rel8
  ( Table(..)
  , InferSchema
  , Schema
  , genericSchema
  , Expr
  , each
  , select
  , where_
  , optional
  , fst_
  , snd_
  , maybe_
  , isNothing
  , catMaybe_
  ) where

import Rel8.Expr
import Rel8.Query
import Rel8.IO
import Rel8.Schema
import Rel8.Table
