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
  )
where

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Prelude hiding ( null, repeat, zipWith )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ) )
import Rel8.Schema.QualifiedName (QualifiedName, showQualifiedName)
import Rel8.Type.Information
  ( TypeInformation(..)
  , parseTypeInformation
  , showTypeName
  )


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array info =
  Opaleye.CastExpr (showQualifiedName (arrayType info) <> "[]") .
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
    , typeName = arrayType info
    , arrayDepth = 1
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
isArray = (> 0) . arrayDepth


arrayType :: TypeInformation a -> QualifiedName
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
        cast = Opaleye.CastExpr (showTypeName info)
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
