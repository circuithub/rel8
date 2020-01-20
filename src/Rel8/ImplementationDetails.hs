-- | This module contains a bunch of symbols that unfortunately leak out of
-- the Rel8 abstraction. __None__ of these concepts should be necessary to
-- use Rel8, but they are exported and documented here for your curiousity.
-- If you find yourself needing to use anything from this module, please
-- open an issue on GitHub - we might be able to find a better solution or
-- add something to the public API for everyone to use instead!

module Rel8.ImplementationDetails
  ( C(..)
  , Compatible( transferField )
  , ConstrainHigherKinded
  , ConstrainTable
  , Field
  , Table
  , GHConstrainTraverse
  , GenericField
  , HConstrainTraverse
  , HField
  , IsTableIn
  , MonadQuery(..)
  , Nest
  , OnConflict
  , Promote
  , Selects
  , Spine
  , Unconstrained
  , aggregateExpr
  , aggregator
  , applyArgument
  , tabulateMCP
  , field
  ) where

import Rel8.Aggregate
import Rel8.Column
import Rel8.Expr
import Rel8.MonadQuery
import Rel8.Nest
import Rel8.Query
import Rel8.SimpleConstraints
import Rel8.Table
import Rel8.Unconstrained
