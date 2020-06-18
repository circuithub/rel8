{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Lit
  ( litTable
  ) where

import Data.Functor.Identity
import Data.Proxy
import Rel8.Column
import Rel8.DBType
import Rel8.Expr
import Rel8.Recontextualise
import Rel8.Table


litTable
  :: ( HConstrainTable (Structure b) (Context b) DBType
     , Context a ~ Identity
     , Context b ~ Expr
     , Recontextualise Lit a b
     )
  => a -> b
litTable = runIdentity . mapContext (Proxy @Lit) (Proxy @DBType) (Identity . mapCC @DBType lit)
