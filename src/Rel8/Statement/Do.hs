{-# language DataKinds #-}
{-# language FlexibleContexts #-}

{-|

This module is intended to be imported qualified as follows:

@
import qualified Rel8.Statement.Do as Statement
@

It's intended to be used wth the @QualifiedDo@ language extension.

-}

module Rel8.Statement.Do
  ( (>>=)    
  , (>>)
  , pure
  , return
  , fmap
  , liftA2
  )
where

-- base
import Data.Function ((.))
import Data.Maybe (Maybe (Just))
import Prelude ()

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Statement
  ( Statement (Select, Bind, Then)
  , IsStatement
  , toStatement
  )
import Rel8.Table (Table)


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
(>>=) :: (IsStatement ('Just a) statement, IsStatement b statement')
  => statement -> (Query a -> statement') -> Statement b
a >>= f = Bind (toStatement a) (toStatement . f)
infixl 1 >>=


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
(>>) :: (IsStatement a statement, IsStatement b statement')
  => statement -> statement' -> Statement b
a >> b = Then (toStatement a) (toStatement b)
infixl 1 >>


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
pure :: Table Expr a => Query a -> Statement ('Just a)
pure = Select


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
return :: Table Expr a => Query a -> Statement ('Just a)
return = Select


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
fmap :: (IsStatement ('Just a) statement, Table Expr b)
  => (Query a -> Query b)
  -> statement
  -> Statement ('Just b)
fmap f m = m >>= pure . f


-- | See 'Statement'. For use with the @QualifiedDo@ extension.
liftA2 :: (IsStatement ('Just a) statement, IsStatement ('Just b) statement', Table Expr c)
  => (Query a -> Query b -> Query c)
  -> statement
  -> statement'
  -> Statement ('Just c)
liftA2 f as bs = as >>= \a -> bs >>= \b -> pure (f a b)
