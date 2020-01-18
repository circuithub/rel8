-- | This module contains a bunch of symbols that unfortunately leak out of
-- the Rel8 abstraction. __None__ of these concepts should be necessary to
-- use Rel8, but they are exported and documented here for your curiousity.
-- If you find yourself needing to use anything from this module, please
-- open an issue on GitHub - we might be able to find a better solution or
-- add something to the public API for everyone to use instead!

module Rel8.ImplementationDetails
  ( C(..)
  , CanZipLeaves
  , ConstrainHigherKinded
  , ExprIn
  , IsTableIn
  , MonadQuery(..)
  , Nest
  , Promote
  , Rewrite( rewrite )
  , Selects
  , Top
  , ZipLeaves( zipLeaves )
  , ZipRecord
  , WFHigherKinded
  , aggregateExpr
  , aggregator
  , zipRecord
  ) where

import Rel8.Aggregate
import Rel8.Column ( C(..) )
import Rel8.HigherKinded
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.Rewrite
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.Top
import Rel8.ZipLeaves
