{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language CPP #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}

module Rel8.Internal.Statement.Prepared
  ( input
  , preparedRun_
  , preparedRunN
  , preparedRun1
  , preparedRunMaybe
  , preparedRun
  , preparedRunVector
) where

-- base
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Functor.Identity (runIdentity)
import Prelude
import Data.Int (Int64)

-- hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Query (Query)
import Rel8.Internal.Expr (Expr)
import Rel8.Internal.Expr.Opaleye (fromPrimExpr, scastExpr)
import Rel8.Internal.Schema.HTable (hfield, hspecs, htabulateA)
import Rel8.Internal.Schema.Null (Nullity (Null, NotNull))
import Rel8.Internal.Schema.Spec (Spec (..))
import Rel8.Internal.Statement (Statement)
import Rel8.Internal.Table (Table, fromColumns, toResult)
import Rel8.Internal.Table.Serialize (Serializable)
import Rel8.Internal.Type.Encoder (binary)
import Rel8.Internal.Type.Information (encode)
import Rel8.Internal.Statement.Rows (Rows (..))
import Rel8.Internal.Statement (Statement, ppDecodeStatement, encodeDoc)
import Rel8.Internal.Statement.Select (ppSelect)

-- text
import qualified Data.Text as Text

-- transformers
import Control.Monad.Trans.State.Strict (evalState, state)

-- vector
import Data.Vector (Vector)

makePreparedRun :: forall a exprs params i. (Serializable params i) => Rows exprs a -> (params -> Statement exprs) -> Hasql.Statement i a
makePreparedRun rows statement = Hasql.preparable bytes (encoder @params) decode 
  where
    bytes = encodeDoc doc
    (doc, decode) = ppDecodeStatement ppSelect rows (statement input)

encoder :: forall a i. Serializable a i => Hasql.Params i
encoder =
  contramap (toResult @_ @a) $
    getConst $
      htabulateA \field ->
        case hfield hspecs field of
          Spec {nullity, info} -> Const $
            runIdentity . (`hfield` field) >$<
              case nullity of
                Null -> Hasql.param $ Hasql.nullable build
                NotNull -> Hasql.param $ Hasql.nonNullable build
              where
                build = binary (encode info)


input :: Table Expr a => a
input =
  fromColumns $
    flip (evalState @Word) 1 do
      htabulateA \field -> do
        n <- state (\n -> (n, n + 1))
        pure
          case hfield hspecs field of
            Spec {info} ->
              scastExpr info $ fromPrimExpr $
                Opaleye.ConstExpr $ Opaleye.OtherLit $ '$' : show n

-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', disregarding the
-- results of that statement (if any).
--
-- @
-- preparedRun_ :: (Serializable params i) => (params -> Rel8.'Statement' exprs) -> Hasql.'Hasql.Statement' i ()
-- @
preparedRun_ :: (Serializable params i) => (params -> Statement exprs) -> Hasql.Statement i ()
preparedRun_ = makePreparedRun Void


-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', returning the
-- number of rows affected by that statement (for 'Rel8.insert's,
-- 'Rel8.update's or Rel8.delete's with 'Rel8.NoReturning').
--
-- @
-- preparedRunN :: (Serializable params i) => (params -> Rel8.'Statement' ()) -> Hasql.'Hasql.Statement' i Int64
-- @
preparedRunN :: (Serializable params i) => (params -> Statement ()) -> Hasql.Statement i Int64
preparedRunN = makePreparedRun RowsAffected


-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', processing the
-- result of the statement as a single row. If the statement returns a number
-- of rows other than 1, a preparedRuntime exception is thrown.
--
-- @
-- preparedRun1 ::(Serializable params i, Serializable exprs a) => (params -> Rel8.'Statement' (Query exprs)) -> Hasql.'Hasql.Statement' i a
-- @
preparedRun1 ::(Serializable params i, Serializable exprs a) => (params -> Statement (Query exprs)) -> Hasql.Statement i a
preparedRun1 = makePreparedRun Single


-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', processing the
-- result of the statement as 'Maybe' a single row. If the statement returns
-- a number of rows other than 0 or 1, a preparedRuntime exception is thrown.
--
-- @
-- preparedRunMaybe :: (Serializable params i, Serializable exprs a) => (params -> Rel8.'Statement' (Query exprs)) -> Hasql.'Hasql.Statement' i (Maybe a)
-- @
preparedRunMaybe :: (Serializable params i, Serializable exprs a)
  => (params -> Statement (Query exprs)) -> Hasql.Statement i (Maybe a)
preparedRunMaybe = makePreparedRun Maybe


-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', processing the
-- result of the statement as a list of rows.
--
-- @
-- preparedRun :: (Serializable params i, Serializable exprs a) => (params -> Rel8.'Statement' (Query exprs)) -> Hasql.'Hasql.Statement' i [a]
-- @
preparedRun :: (Serializable params i, Serializable exprs a)
  => (params -> Statement (Query exprs)) -> Hasql.Statement i [a]
preparedRun = makePreparedRun List


-- | Convert a 'Statement' to a prepared runnable 'Hasql.Statement', processing the
-- result of the statement as a 'Vector' of rows.
--
-- @
-- preparedRunVector :: (Serializable params i, Serializable exprs a) => (params -> Rel8.'Statement' (Query exprs)) -> Hasql.'Hasql.Statement' i (Vector a)
-- @
preparedRunVector :: (Serializable params i, Serializable exprs a)
  => (params -> Statement (Query exprs)) -> Hasql.Statement i (Vector a)
preparedRunVector = makePreparedRun Vector
