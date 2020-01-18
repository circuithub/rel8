{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Nest where


-- | @Nest m@ adds a new distinct scope to the monad @m@. Expressions produced
-- in @m@ cannot be used in @Nest m@ (and vice versa).
newtype Nest m a = Nest ( m a )
  deriving ( Functor, Applicative, Monad )
