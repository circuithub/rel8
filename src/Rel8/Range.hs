module Rel8.Range (
  -- * Basic range functionality
  Bound (Incl, Excl, Inf),
  Range (Empty, Range),
  Multirange (Multirange),
  range,
  multirange,
  rangeAgg,
 
  -- * Defining new range types
  DBRange (
    rangeTypeName, rangeDecoder, rangeEncoder,
    multirangeTypeName, multirangeDecoder, multirangeEncoder
  )
) where

-- base
import Prelude ()

-- rel8
import Rel8.Aggregate.Range (rangeAgg)
import Rel8.Data.Range (
  Bound (Incl, Excl, Inf),
  Range (Empty, Range),
  Multirange (Multirange),
 )
import Rel8.Expr.Range (range, multirange)
import Rel8.Type.Range (
  DBRange (
    rangeTypeName, rangeDecoder, rangeEncoder,
    multirangeTypeName, multirangeDecoder, multirangeEncoder
  ),
 )
