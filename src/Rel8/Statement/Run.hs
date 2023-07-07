module Rel8.Statement.Run (
  run_,
  runN,
  run1,
  runMaybe,
  run,
  runVector,
)
where

-- base
import Data.Int (Int64)
import Prelude

-- hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import Rel8.Query (Query)
import Rel8.Statement (Statement, ppDecodeStatement)
import Rel8.Statement.Rows (Rows (..))
import Rel8.Statement.Select (ppSelect)
import Rel8.Table.Serialize (Serializable)

-- text
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

-- vector
import Data.Vector (Vector)


makeRun :: Rows exprs a -> Statement exprs -> Hasql.Statement () a
makeRun rows statement = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    prepare = False
    sql = show doc
    (doc, decode) = ppDecodeStatement ppSelect rows statement


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', disregarding the
results of that statement (if any).
-}
run_ :: Statement exprs -> Hasql.Statement () ()
run_ = makeRun Void


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', returning the
number of rows affected by that statement (for 'Rel8.insert's,
'Rel8.update's or Rel8.delete's with 'Rel8.NoReturning').
-}
runN :: Statement () -> Hasql.Statement () Int64
runN = makeRun RowsAffected


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', processing the
result of the statement as a single row. If the statement returns a number
of rows other than 1, a runtime exception is thrown.
-}
run1 ::
  Serializable
    exprs
    a =>
  Statement (Query exprs) ->
  Hasql.Statement () a
run1 = makeRun Single


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', processing the
result of the statement as 'Maybe' a single row. If the statement returns
a number of rows other than 0 or 1, a runtime exception is thrown.
-}
runMaybe ::
  Serializable
    exprs
    a =>
  Statement (Query exprs) ->
  Hasql.Statement () (Maybe a)
runMaybe = makeRun Maybe


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', processing the
result of the statement as a list of rows.
-}
run ::
  Serializable exprs a =>
  Statement (Query exprs) ->
  Hasql.Statement () [a]
run = makeRun List


{- | Convert a 'Statement' to a runnable 'Hasql.Statement', processing the
result of the statement as a 'Vector' of rows.
-}
runVector ::
  Serializable exprs a =>
  Statement (Query exprs) ->
  Hasql.Statement () (Vector a)
runVector = makeRun Vector
