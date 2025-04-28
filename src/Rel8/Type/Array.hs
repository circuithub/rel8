{-# language DisambiguateRecordFields #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Rel8.Type.Array
  ( array, quoteArrayElement, extractArrayElement
  , arrayTypeName
  , listTypeInformation
  , nonEmptyTypeInformation
  , head, index, last, length
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), many)
import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.Functor.Contravariant ((>$<))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Prelude hiding (head, last, length, null, repeat, zipWith)

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L

-- case-insensitive
import qualified Data.CaseInsensitive as CI

-- hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null (Unnullify, Nullity (Null, NotNull))
import Rel8.Type.Builder.Fold (interfoldMap)
import Rel8.Type.Decoder (Decoder (..), Parser)
import Rel8.Type.Encoder (Encoder (..))
import Rel8.Type.Information (TypeInformation(..), parseTypeInformation)
import Rel8.Type.Name (TypeName (..), showTypeName)
import Rel8.Type.Nullable (NullableOrNot (..))
import Rel8.Type.Parser (parse)

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Encoding as Lazy (decodeUtf8)


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array info =
  Opaleye.CastExpr (showTypeName (arrayType info) <> "[]") .
  Opaleye.ArrayExpr . map (quoteArrayElement info) . toList
{-# INLINABLE array #-}


listTypeInformation :: ()
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation [a]
listTypeInformation nullity info@TypeInformation {decode, encode, delimiter} =
  TypeInformation
    { decode =
        Decoder
          { binary = Decoders.listArray $ case nullity of
              Null -> Decoders.nullable (decodeArrayElement info decode)
              NotNull -> Decoders.nonNullable (decodeArrayElement info decode)
          , text = case nullity of
              Null -> arrayParser delimiter (Nullable decode)
              NotNull -> arrayParser delimiter (NonNullable decode)
          }
    , encode =
        Encoder
          { binary = Encoders.foldableArray $ case nullity of
              Null -> Encoders.nullable (encodeArrayElement info encode)
              NotNull -> Encoders.nonNullable (encodeArrayElement info encode)
          , text = case nullity of
              Null -> arrayBuild delimiter (Nullable encode)
              NotNull -> arrayBuild delimiter (NonNullable encode)
          , quote = case nullity of
              Null ->
                Opaleye.ArrayExpr .
                fmap (quoteArrayElement info . maybe null (quote encode))
              NotNull ->
                Opaleye.ArrayExpr .
                fmap (quoteArrayElement info . quote encode)
          }
    , delimiter = ','
    , typeName = arrayTypeName info
    }
  where
    null = Opaleye.ConstExpr Opaleye.NullLit


nonEmptyTypeInformation :: ()
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation (NonEmpty a)
nonEmptyTypeInformation nullity =
  parseTypeInformation fromList toList . listTypeInformation nullity
  where
    fromList = maybe (Left message) Right . nonEmpty
    message = "failed to decode NonEmptyList: got empty list"


arrayTypeName :: TypeInformation a -> TypeName
arrayTypeName info = (arrayType info) {arrayDepth = 1}


isArray :: TypeInformation a -> Bool
isArray = (> 0) . arrayDepth . typeName


arrayType :: TypeInformation a -> TypeName
arrayType info
  | isArray info = "text"
  | otherwise = typeName info


decodeArrayElement :: TypeInformation a -> Decoder x -> Decoders.Value x
decodeArrayElement info Decoder {binary, text}
  | isArray info =
      Decoders.refine (first Text.pack . text) Decoders.bytea
  | otherwise = binary


encodeArrayElement :: TypeInformation a -> Encoder x -> Encoders.Value x
encodeArrayElement info Encoder {binary, text}
  | isArray info = Text.toStrict . Lazy.decodeUtf8 . toLazyByteString . text >$< Encoders.text
  | otherwise = binary


quoteArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
quoteArrayElement info
  | isArray info = Opaleye.CastExpr "text" . Opaleye.CastExpr (showTypeName (typeName info))
  | otherwise = id


extractArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
extractArrayElement info
  | isArray info = Opaleye.CastExpr (showTypeName (typeName info))
  | otherwise = id


parseArray :: Char -> ByteString -> Either String [Maybe ByteString]
parseArray delimiter = parse $ do
  A.char '{' *> A.sepBy element (A.char delimiter) <* A.char '}'
  where
    element = null <|> nonNull
      where
        null = Nothing <$ A.string "NULL"
        nonNull = Just <$> (quoted <|> unquoted)
          where
            unquoted = A.takeWhile1 (A.notInClass (delimiter : "\"{}"))
            quoted = A.char '"' *> contents <* A.char '"'
              where
                contents = fold <$> many (unquote <|> unescape)
                  where
                    unquote = A.takeWhile1 (A.notInClass "\"\\")
                    unescape = A.char '\\' *> do
                      BS.singleton <$> do
                        A.char '\\' <|> A.char '"'


arrayParser :: Char -> NullableOrNot Decoder a -> Parser [a]
arrayParser delimiter = \case
  Nullable Decoder {text} -> \input -> do
    elements <- parseArray delimiter input
    traverse (traverse text) elements
  NonNullable Decoder {text} -> \input -> do
    elements <- parseArray delimiter input
    traverse (maybe (Left "array: unexpected null") text) elements


buildArray :: Char -> [Maybe ByteString] -> Builder
buildArray delimiter elements =
  B.char8 '{' <>
  interfoldMap (B.char8 delimiter) element elements <>
  B.char8 '}'
  where
    element = \case
      Nothing -> B.string7 "NULL"
      Just a
        | BS.null a -> "\"\""
        | CI.mk a == "null" -> escaped
        | BS.any (A.inClass (delimiter : "\\\"{}")) a -> escaped
        | otherwise -> unescaped
        where
          unescaped = B.byteString a
          escaped =
            B.char8 '"' <> BS.foldr ((<>) . escape) mempty a <> B.char8 '"'
            where
              escape = \case
                '"' -> B.string7 "\\\""
                '\\' -> B.string7 "\\\\"
                c -> B.char8 c


arrayBuild :: Char -> NullableOrNot Encoder a -> [a] -> Builder
arrayBuild delimiter = \case
  Nullable Encoder {text} ->
    buildArray delimiter .
    map (fmap (L.toStrict . toLazyByteString . text))
  NonNullable Encoder {text} ->
    buildArray delimiter .
    map (Just . L.toStrict . toLazyByteString . text)


head :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
head info a = extractArrayElement info $ subscript (lower a) a


last :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
last info a = extractArrayElement info $ subscript (upper a) a


subscript :: Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr
subscript i a = Opaleye.ArrayIndex a i


index :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr
index info i a = extractArrayElement info $ subscript (plus (lower a) i) a


lower :: Opaleye.PrimExpr -> Opaleye.PrimExpr
lower a = Opaleye.FunExpr "array_lower" [a, one]


upper :: Opaleye.PrimExpr -> Opaleye.PrimExpr
upper a = Opaleye.FunExpr "array_lower" [a, one]


length :: Opaleye.PrimExpr -> Opaleye.PrimExpr
length a = Opaleye.FunExpr "coalesce" [Opaleye.FunExpr "array_length" [a, one], zero]


one :: Opaleye.PrimExpr
one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)


zero :: Opaleye.PrimExpr
zero = Opaleye.ConstExpr (Opaleye.IntegerLit 0)


plus :: Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr
plus = Opaleye.BinExpr (Opaleye.:+)
