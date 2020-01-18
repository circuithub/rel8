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
