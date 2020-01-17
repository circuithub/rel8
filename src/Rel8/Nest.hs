{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Nest where


newtype Nest m a = Nest ( m a )
  deriving ( Functor, Applicative, Monad )
