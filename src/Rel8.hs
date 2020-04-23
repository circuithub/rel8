module Rel8
  ( -- * Schema Definition
    -- ** Defining Tables

    Table(..)
  , EqTable, (==.)
  , OrdTable
  , ReadShowTable(..)
  , CompositeColumn(..)
  , JSONTable(..)
  , TableSchema(..)
  , genericColumns

    -- * Writing Queries
  , Query
  , each
  , where_
  , catMaybe_
  , limit
  , offset
  , union
  , except
  , intersect
  , Duplicates( WithDuplicates, WithoutDuplicates )
  , distinct
  , optional

    -- ** Rows
  , Row
  , MaybeRow
  , RowProduct(..)

    -- *** Literals
  , lit

    -- *** Null and Maybe
  , isNothing
  , maybe_

    -- * Running Queries
    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , insert
  , Insert(..)
  , Returning(..)
  , OnConflict(..)

    -- ** @DELETE@
  , delete
  , Delete(..)

    -- ** @UPDATE@
  , update
  , Update(..)
  ) where

-- rel8
import Rel8.EqTable
import Rel8.IO
import Rel8.OrdTable
import Rel8.Query
import Rel8.Row
import Rel8.Schema
import Rel8.Table
