{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Rel8.Type.Array
  ( array, encodeArrayElement, extractArrayElement
  , listTypeInformation
  , nonEmptyTypeInformation
  , head, last, length
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), many)
import Data.Bifunctor (first)
import Data.Foldable (fold, toList)
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Prelude hiding ( head, last, length, null, repeat, zipWith )

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ) )
import Rel8.Type.Decoder (Decoder (..), NullableOrNot (..), Parser)
import Rel8.Type.Information ( TypeInformation(..), parseTypeInformation )
import Rel8.Type.Name (TypeName (..), showTypeName)
import Rel8.Type.Parser (parse)

-- text
import qualified Data.Text as Text


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array info =
  Opaleye.CastExpr (showTypeName (arrayType info) <> "[]") .
  Opaleye.ArrayExpr . map (encodeArrayElement info) . toList
{-# INLINABLE array #-}


listTypeInformation :: ()
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation [a]
listTypeInformation nullity info@TypeInformation {encode, decode} =
  TypeInformation
    { decode =
        Decoder
          { binary = Hasql.listArray $ case nullity of
              Null -> Hasql.nullable (decodeArrayElement info decode)
              NotNull -> Hasql.nonNullable (decodeArrayElement info decode)
          , parser = case nullity of
              Null -> arrayParser (Nullable decode)
              NotNull -> arrayParser (NonNullable decode)
          , delimiter = ','
          }
    , encode = case nullity of
        Null ->
          Opaleye.ArrayExpr .
          fmap (encodeArrayElement info . maybe null encode)
        NotNull ->
          Opaleye.ArrayExpr .
          fmap (encodeArrayElement info . encode)
    , typeName = (arrayType info) {arrayDepth = 1}
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


isArray :: TypeInformation a -> Bool
isArray = (> 0) . arrayDepth . typeName


arrayType :: TypeInformation a -> TypeName
arrayType info
  | isArray info = "text"
  | otherwise = typeName info


decodeArrayElement :: TypeInformation a -> Decoder x -> Hasql.Value x
decodeArrayElement info
  | isArray info = \decoder ->
      Hasql.refine (first Text.pack . parser decoder) Hasql.bytea
  | otherwise = binary


encodeArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
encodeArrayElement info
  | isArray info = Opaleye.CastExpr "text"
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


arrayParser :: NullableOrNot Decoder a -> Parser [a]
arrayParser = \case
  Nullable Decoder {parser, delimiter} -> \input -> do
    elements <- parseArray delimiter input
    traverse (traverse parser) elements
  NonNullable Decoder {parser, delimiter} -> \input -> do
    elements <- parseArray delimiter input
    traverse (maybe (Left "array: unexpected null") parser) elements


head :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
head info a = extractArrayElement info $ index (lower a) a


last :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
last info a = extractArrayElement info $ index (upper a) a


index :: Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr
index i a = Opaleye.ArrayIndex a i


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
