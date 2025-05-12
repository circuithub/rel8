{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Prepared (
  input,
  prepared,
) where

-- base
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Functor.Identity (runIdentity)
import Prelude

-- hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, scastExpr)
import Rel8.Schema.HTable (hfield, hspecs, htabulateA)
import Rel8.Schema.Null (Nullity (Null, NotNull))
import Rel8.Schema.Spec (Spec (..))
import Rel8.Statement (Statement)
import Rel8.Table (Table, fromColumns, toResult)
import Rel8.Table.Serialize (Serializable)
import Rel8.Type.Encoder (binary)
import Rel8.Type.Information (encode)

-- transformers
import Control.Monad.Trans.State.Strict (evalState, state)


{-| Given a 'Rel8.run' function that converts a 'Statement' to a
'Hasql.Statement', return a 'Rel8.run'-like function which instead takes a
/parameterized/ 'Statement' and converts it to a /preparable/
'Hasql.Statement'.

The parameters @i@ are sent to the database directly via PostgreSQL's binary
format. For large amounts of data this can be significantly more efficient
than embedding the values in the statement with 'Rel8.lit'.
-}
prepared :: forall a b i o.
  Serializable a i =>
  (Statement b -> Hasql.Statement () o) ->
  (a -> Statement b) ->
  Hasql.Statement i o
prepared run mkStatement = Hasql.Statement sql (encoder @a) decode True
  where
    Hasql.Statement sql _ decode _ = run $ mkStatement input


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
