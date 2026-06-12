{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rel8.Data.Range (
  Bound (Incl, Excl, Inf),
  Range (Empty, Range),
  quoteRange,
  mapRange,
  Multirange (Multirange),
  primMultirange,
) where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative (many, optional, (<|>))
import Control.Monad ((>=>))
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Functor.Contravariant ((>$<))
import Prelude

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L

-- hasql
import qualified Hasql.Decoders as Decoder
import qualified Hasql.Encoders as Encoder

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- postgresql-binary
import PostgreSQL.Binary.Range (Bound (Incl, Excl, Inf), Range (Empty, Range))
import qualified PostgreSQL.Binary.Range as PostgreSQL

-- rel8
import Rel8.Schema.QualifiedName (QualifiedName, showQualifiedName)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Builder.Fold (interfoldMap)
import Rel8.Type.Decoder (Decoder (Decoder))
import qualified Rel8.Type.Decoder
import Rel8.Type.Encoder (Encoder (Encoder))
import qualified Rel8.Type.Encoder
import Rel8.Type.Eq (DBEq)
import Rel8.Type.Information (TypeInformation (TypeInformation))
import qualified Rel8.Type.Information
import Rel8.Type.Name (TypeName (TypeName))
import qualified Rel8.Type.Name
import Rel8.Type.Ord (DBOrd)
import Rel8.Type.Range (
  DBRange,
  rangeTypeName, rangeEncoder, rangeDecoder,
  multirangeTypeName, multirangeEncoder, multirangeDecoder,
 )
import Rel8.Type.Parser (parse)


newtype Multirange a = Multirange (PostgreSQL.Multirange a)
  deriving (Eq, Ord, Show)


instance DBRange a => DBType (Range a) where
  typeInformation =
    rangeTypeInformation name rangeEncoder rangeDecoder element
    where
      name = rangeTypeName @a
      element = typeInformation @a


instance DBRange a => DBEq (Range a)


instance DBRange a => DBOrd (Range a)


instance DBRange a => DBType (Multirange a) where
  typeInformation =
    multirangeTypeInformation
      multiname
      name
      multirangeEncoder
      multirangeDecoder
      element
    where
      multiname = multirangeTypeName @a
      name = rangeTypeName @a
      element = typeInformation @a


instance DBRange a => DBEq (Multirange a)


instance DBRange a => DBOrd (Multirange a)


rangeTypeInformation ::
  QualifiedName ->
  Encoder.Value (Range a) ->
  Decoder.Value (Range a) ->
  TypeInformation a ->
  TypeInformation (Range a)
rangeTypeInformation name encoder decoder element =
  TypeInformation
    { encode =
        Encoder
          { binary = encoder
          , text = buildRange . mapRange (render . element.encode.text)
          , quote = quoteRange name . mapRange element.encode.quote
          }
    , decode =
        Decoder
          { binary = decoder
          , text = parseRange >=> traverseRange element.decode.text
          }
    , delimiter = ','
    , typeName =
        TypeName
          { name
          , modifiers = []
          , arrayDepth = 0
          }
    }
  where
    render = L.toStrict . toLazyByteString


multirangeTypeInformation ::
  QualifiedName ->
  QualifiedName ->
  Encoder.Value (PostgreSQL.Multirange a) ->
  Decoder.Value (PostgreSQL.Multirange a) ->
  TypeInformation a ->
  TypeInformation (Multirange a)
multirangeTypeInformation multiname name encoder decoder element =
  TypeInformation
    { encode =
        Encoder
          { binary = (\(Multirange ranges) -> ranges) >$< encoder
          , text =
              buildMultirange . mapMultirange (render . element.encode.text)
          , quote =
              quoteMultirange multiname name
                . mapMultirange element.encode.quote
          }
    , decode =
        Decoder
          { binary = Multirange <$> decoder
          , text = parseMultirange >=> traverseMultirange element.decode.text
          }
    , delimiter = ','
    , typeName =
        TypeName
          { name = multiname
          , modifiers = []
          , arrayDepth = 0
          }
    }
  where
    render = L.toStrict . toLazyByteString


buildRange :: Range ByteString -> Builder
buildRange = \case
  Empty -> B.string7 "empty"
  Range lo hi -> lower <> B.char8 ',' <> upper
    where
      lower = case lo of
        Incl a -> B.char8 '[' <> element a
        Excl a -> B.char8 '(' <> element a
        Inf -> B.char8 '('
      upper = case hi of
        Incl a -> element a <> B.char8 ']'
        Excl a -> element a <> B.char8 ')'
        Inf -> B.char8 ')'
  where
    element bytes
      | BS.null bytes = B.string7 "\"\""
      | BS.any (A.inClass escapeClass) bytes = escape bytes
      | otherwise = B.byteString bytes
    escapeClass = ",()[]\\\" \t\n\r\v\f"
    escape bytes =
      B.char8 '"' <> BS.foldr ((<>) . go) mempty bytes <> B.char8 '"'
      where
        go = \case
          '"' -> B.string7 "\\\""
          '\\' -> B.string7 "\\\\"
          c -> B.char8 c


quoteRange :: QualifiedName -> Range Opaleye.PrimExpr -> Opaleye.PrimExpr
quoteRange name = \case
  Empty ->
    Opaleye.ConstExpr (Opaleye.StringLit "empty")
  Range lo hi ->
    Opaleye.FunExpr constructor [lower, upper, bounds]
    where
      lower = case lo of
        Incl a -> a
        Excl a -> a
        Inf -> Opaleye.ConstExpr Opaleye.NullLit
      upper = case hi of
        Incl a -> a
        Excl a -> a
        Inf -> Opaleye.ConstExpr Opaleye.NullLit
      bounds = Opaleye.ConstExpr (Opaleye.StringLit (l : h : []))
        where
          l = case lo of
            Incl _ -> '['
            _ -> '('
          h = case hi of
            Incl _ -> ']'
            _ -> ')'
  where
    constructor = showQualifiedName name


parseRange :: ByteString -> Either String (Range ByteString)
parseRange = parse $ empty <|> nonEmpty
  where
    empty = Empty <$ A.string "empty"
    nonEmpty = rangeParser


rangeParser :: A.Parser (Range ByteString)
rangeParser = do
  lo <- Incl <$ A.char '[' <|> Excl <$ A.char '('
  mlower <- optional element
  void $ A.char ','
  mupper <- optional element
  hi <- Incl <$ A.char ']' <|> Excl <$ A.char ')'
  let
    lower = maybe Inf lo mlower
    upper = maybe Inf hi mupper
  pure $ Range lower upper
  where
    element = quoted <|> unquoted
      where
        unquoted = A.takeWhile1 (A.notInClass ",)]")
        quoted = A.char '"' *> contents <* A.char '"'
          where
            contents = fold <$> many (unquote <|> unescape)
              where
                unquote = A.takeWhile1 (A.notInClass "\"\\")
                unescape = A.char '\\' *> do
                  BS.singleton <$> do
                    A.char '\\' <|> A.char '"'


buildMultirange :: Multirange ByteString -> Builder
buildMultirange (Multirange ranges) =
  B.char8 '{' <> interfoldMap (B.char8 ',') buildRange ranges <> B.char8 '}'


quoteMultirange ::
  QualifiedName ->
  QualifiedName ->
  Multirange Opaleye.PrimExpr ->
  Opaleye.PrimExpr
quoteMultirange multiname name (Multirange ranges) =
  primMultirange multiname (map (cast . quoteRange name) ranges)
  where
    cast = Opaleye.CastExpr (showQualifiedName name)


primMultirange :: QualifiedName -> [Opaleye.PrimExpr] -> Opaleye.PrimExpr
primMultirange = Opaleye.FunExpr . showQualifiedName


parseMultirange ::
  ByteString ->
  Either String (Multirange ByteString)
parseMultirange =
  parse $
    Multirange <$> do
      A.char '{' *> A.sepBy rangeParser (A.char ',') <* A.char '}'


mapBound :: (a -> b) -> Bound a -> Bound b
mapBound f = \case
  Incl a -> Incl (f a)
  Excl a -> Excl (f a)
  Inf -> Inf


traverseBound ::
  Applicative f =>
  (a -> f b) ->
  Bound a ->
  f (Bound b)
traverseBound f = \case
  Incl a -> Incl <$> f a
  Excl a -> Excl <$> f a
  Inf -> pure Inf


mapRange :: (a -> b) -> Range a -> Range b
mapRange f = \case
  Empty -> Empty
  Range a b -> Range (mapBound f a) (mapBound f b)


traverseRange ::
  Applicative f =>
  (a -> f b) ->
  Range a ->
  f (Range b)
traverseRange f = \case
  Empty -> pure Empty
  Range a b ->
    Range <$> traverseBound f a <*> traverseBound f b


mapMultirange ::
  (a -> b) ->
  Multirange a ->
  Multirange b
mapMultirange f (Multirange ranges) = Multirange (map (mapRange f) ranges)


traverseMultirange ::
  Applicative f =>
  (a -> f b) ->
  Multirange a ->
  f (Multirange b)
traverseMultirange f (Multirange ranges) =
  Multirange <$> traverse (traverseRange f) ranges
