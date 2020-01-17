{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Nest where

import {-# source #-} Rel8.MonadQuery


newtype Nest m a = Nest ( m a )
  deriving ( Functor, Applicative, Monad, MonadQuery )
