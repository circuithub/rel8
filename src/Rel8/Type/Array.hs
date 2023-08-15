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

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Prelude hiding ( head, last, length, null, repeat, zipWith )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ) )
import Rel8.Type.Information ( TypeInformation(..), parseTypeInformation )
import Rel8.Type.Name (TypeName (..), showTypeName)


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
    { decode = case nullity of
        Null ->
          Hasql.listArray (decodeArrayElement info (Hasql.nullable decode))
        NotNull ->
          Hasql.listArray (decodeArrayElement info (Hasql.nonNullable decode))
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
  parseTypeInformation parse toList . listTypeInformation nullity
  where
    parse = maybe (Left message) Right . nonEmpty
    message = "failed to decode NonEmptyList: got empty list"


isArray :: TypeInformation a -> Bool
isArray = (> 0) . arrayDepth . typeName


arrayType :: TypeInformation a -> TypeName
arrayType info
  | isArray info = "record"
  | otherwise = typeName info


decodeArrayElement :: TypeInformation a -> Hasql.NullableOrNot Hasql.Value x -> Hasql.NullableOrNot Hasql.Value x
decodeArrayElement info
  | isArray info = Hasql.nonNullable . Hasql.composite . Hasql.field
  | otherwise = id


encodeArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
encodeArrayElement info
  | isArray info = Opaleye.UnExpr (Opaleye.UnOpOther "ROW")
  | otherwise = id


extractArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
extractArrayElement info
  | isArray info = extract
  | otherwise = id
  where
    extract input = cast unrow
      where
        string = Opaleye.ConstExpr . Opaleye.StringLit
        int = Opaleye.ConstExpr . Opaleye.IntegerLit . toInteger @Int
        minus a b = Opaleye.BinExpr (Opaleye.:-) a b
        len = Opaleye.FunExpr "length" . pure
        substr s a b = Opaleye.FunExpr "substr" [s, a, b]
        cast = Opaleye.CastExpr (showTypeName (typeName info))
        text = Opaleye.CastExpr "text" input
        unrow =
          Opaleye.CaseExpr
            [ (quoted, unquote)
            ]
            unparen
          where
            quoted = Opaleye.BinExpr Opaleye.OpLike text pattern
              where
                pattern = string "(\"%\")"
        unparen = unwrap 1
        unwrap n = substr text (int (1 + n)) (minus (len text) (int (n * 2)))
        unquote = unescape '"' $ unescape '\\' $ unwrap 2
          where
            unescape char a =
              Opaleye.FunExpr "replace" [a, pattern, replacement]
                where
                  pattern = string [char, char]
                  replacement = string [char]


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
length a = Opaleye.FunExpr "array_length" [a, one]


one :: Opaleye.PrimExpr
one = Opaleye.ConstExpr (Opaleye.IntegerLit 1)