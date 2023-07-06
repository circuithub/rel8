{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.With
  ( With(..)
  , runWith
  , withInsert
  , withQuery
  , bindWith
  , ppWith
  , ppWithBinding
  , withQuery
  , showWith
  )
where

-- base
import Data.Foldable (fold, toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, intersperse)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint
  ( Doc
  , (<+>)
  , ($$)
  , comma
  , doubleQuotes
  , hcat
  , parens
  , punctuate
  , text
  , vcat
  )

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Query.Each (each)
import Rel8.Schema.Table (TableSchema (..))
import Rel8.Table (Table)
import Rel8.Table.Cols (fromCols)
import Rel8.Table.Name (namesFromLabelsWithA, showNames)
import Rel8.Table.Serialize ( Serializable, parse )
import Rel8.Statement.Insert (Insert, ppInsert)
import Rel8.Statement.Select (ppSelect)

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState, modify', runState, state)


type With :: Type -> Type
newtype With a = With (State WithState a)
  deriving newtype (Functor, Applicative, Monad)


withInsert :: Table Expr a => Insert (Query a) -> With (Query a)
withInsert insert = bindWith (evalState (ppInsert insert) Opaleye.start)

withQuery :: Table Expr a => Query a -> With (Query a)
withQuery query = bindWith (evalState (ppSelect query) Opaleye.start)


data WithState = WithState
  { tag :: Opaleye.Tag
  , bindings :: [(Doc, Doc)]
  }


initialWithState :: WithState
initialWithState = WithState Opaleye.start []


runWith :: forall exprs a. Serializable exprs a => With (Query exprs) -> Hasql.Statement () [a]
runWith (With m) = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 (Text.pack sql)
    params = Hasql.noParams
    decode = Hasql.rowList (parse @exprs @a)
    prepare = False
    sql = show doc
    doc = ppWith (reverse (bindings s')) (evalState (ppSelect query) (tag s'))
      where
        (query, s') = runState m initialWithState


showWith :: Table Expr exprs => With (Query exprs) -> String
showWith (With m) = show doc
  where
    doc = ppWith (bindings s') (evalState (ppSelect query) (tag s'))
      where
        (query, s') = runState m initialWithState


bindWith :: Table Expr a => Doc -> With (Query a)
bindWith q = With do
  tag <- state \s -> let t = tag s in (t, s{tag = Opaleye.next t})
  let
    symbol labels = do
      subtag <- Opaleye.fresh
      let
        suffix = Opaleye.tagWith tag (Opaleye.tagWith subtag "")
      pure $ take (63 - length suffix) label ++ suffix
      where
        label = fold (intersperse "/" labels)
    names = namesFromLabelsWithA symbol `evalState` Opaleye.start
    relation = Opaleye.tagWith tag "statement"
    columns = showNames names
    query =
      fromCols <$> each
        TableSchema
          { name = relation
          , schema = Nothing
          , columns = names
          }
  modify' \s -> s{bindings = (ppWithBinding relation columns, q):bindings s}
  pure query


ppWith :: [(Doc, Doc)] -> Doc -> Doc
ppWith withs after = pre $$ after
  where
    pre = case withs of
      [] -> mempty
      _ ->
        text "WITH" <+>
        vcat (punctuate comma (map go withs))
    go (with, before) =
      with $$
      text "AS" <+>
      parens before


ppWithBinding :: String -> NonEmpty String -> Doc
ppWithBinding relation columns =
  escape relation <+>
  parens (hcat (punctuate comma (escape <$> toList columns)))


escape :: String -> Doc
escape = doubleQuotes . text . concatMap go
  where
    go = \case
      '"' -> "\"\""
      c -> [c]