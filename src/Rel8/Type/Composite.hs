{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Type.Composite
  ( Composite( Composite )
  , DBComposite( compositeFields, compositeTypeName )
  , compose, decompose
  , decodeComposite, encodeComposite
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), many, optional)
import Data.Foldable (fold)
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Kind ( Constraint, Type )
import Data.List (uncons)
import Prelude

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)

-- hasql
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( castExpr, fromPrimExpr, toPrimExpr )
import Rel8.Schema.HTable (HTable, hfield, hfoldMap, hspecs, htabulate, htabulateA)
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.QualifiedName (QualifiedName)
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec, nullity, info ) )
import Rel8.Table ( fromColumns, toColumns, fromResult, toResult )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.HKD ( HKD, HKDable )
import Rel8.Table.Ord ( OrdTable )
import Rel8.Table.Rel8able ()
import Rel8.Table.Serialize ( litHTable )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Builder.Fold (interfoldMap)
import Rel8.Type.Decoder (Decoder (Decoder), Parser)
import qualified Rel8.Type.Decoder as Decoder
import Rel8.Type.Encoder (Encoder (Encoder))
import qualified Rel8.Type.Encoder as Encoder
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Name (TypeName (..))
import Rel8.Type.Ord ( DBOrd, DBMax, DBMin )
import Rel8.Type.Parser (parse)

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (StateT), runStateT)


-- | A deriving-via helper type for column types that store a Haskell product
-- type in a single Postgres column using a Postgres composite type.
--
-- Note that this must map to a specific extant type in your database's schema
-- (created with @CREATE TYPE@). Use 'DBComposite' to specify the name of this
-- Postgres type and the names of the individual fields (for projecting with
-- 'decompose').
type Composite :: Type -> Type
newtype Composite a = Composite
  { unComposite :: a
  }


decodeComposite :: HTable t => Decoder (t Result)
decodeComposite =
  Decoder
    { binary = Decoders.composite decoder
    , text = parser
    }


encodeComposite :: forall t. HTable t => Encoder (t Result)
encodeComposite =
  Encoder
    { binary = Encoders.composite (encoder @t)
    , text = builder
    , quote = quoter . litHTable
    }


instance DBComposite a => DBType (Composite a) where
  typeInformation = TypeInformation
    { decode = Composite . fromResult @_ @(HKD a Expr) <$> decodeComposite
    , encode = toResult @_ @(HKD a Expr) . unComposite >$< encodeComposite
    , delimiter = ','
    , typeName =
        TypeName
          { name = compositeTypeName @a
          , modifiers = []
          , arrayDepth = 0
          }
    }


instance (DBComposite a, EqTable (HKD a Expr)) => DBEq (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBOrd (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBMax (Composite a)


instance (DBComposite a, OrdTable (HKD a Expr)) => DBMin (Composite a)


-- | 'DBComposite' is used to associate composite type metadata with a Haskell
-- type.
type DBComposite :: Type -> Constraint
class (DBType a, HKDable a) => DBComposite a where
  -- | The names of all fields in the composite type that @a@ maps to.
  compositeFields :: HKD a Name

  -- | The name of the composite type that @a@ maps to.
  compositeTypeName :: QualifiedName


-- | Collapse a 'HKD' into a PostgreSQL composite type.
--
-- 'HKD' values are represented in queries by having a column for each field in
-- the corresponding Haskell type. 'compose' collapses these columns into a
-- single column expression, by combining them into a PostgreSQL composite
-- type.
compose :: DBComposite a => HKD a Expr -> Expr a
compose = castExpr . fromPrimExpr . quoter . toColumns


-- | Expand a composite type into a 'HKD'.
--
-- 'decompose' is the inverse of 'compose'.
decompose :: forall a. DBComposite a => Expr a -> HKD a Expr
decompose (toPrimExpr -> a) = fromColumns $ htabulate \field ->
  case hfield names field of
    Name name -> case hfield hspecs field of
      Spec {} -> fromPrimExpr $ Opaleye.CompositeExpr a name
  where
    names = toColumns (compositeFields @a)


decoder :: HTable t => Decoders.Composite (t Result)
decoder = unwrapApplicative $ htabulateA \field ->
  case hfield hspecs field of
    Spec {nullity, info} -> WrapApplicative $ Identity <$>
      case nullity of
        Null -> Decoders.field $ Decoders.nullable $ Decoder.binary $ decode info
        NotNull -> Decoders.field $ Decoders.nonNullable $ Decoder.binary $ decode info


parser :: HTable t => Parser (t Result)
parser input = do
  fields <- parseRow input
  (a, rest) <- runStateT go fields
  case rest of
    [] -> pure a
    _ -> Left "composite: too many fields"
  where
    go = htabulateA \field -> do
      mbytes <- StateT $ maybe missing pure . uncons
      lift $ Identity <$> case hfield hspecs field of
        Spec {nullity, info} -> case nullity of
          Null -> traverse (Decoder.text (decode info)) mbytes
          NotNull -> case mbytes of
            Nothing -> Left "composite: unexpected null"
            Just bytes -> Decoder.text (decode info) bytes
    missing = Left "composite: missing fields"


parseRow :: ByteString -> Either String [Maybe ByteString]
parseRow = parse $ do
  A.char '(' *> A.sepBy element (A.char ',') <* A.char ')'
  where
    element = optional (quoted <|> unquoted)
      where
        unquoted = A.takeWhile1 (A.notInClass ",\"()")
        quoted = A.char '"' *> contents <* A.char '"'
          where
            contents = fold <$> many (unquote <|> unescape <|> quote)
              where
                unquote = A.takeWhile1 (A.notInClass "\"\\")
                unescape = A.char '\\' *> do
                  BS.singleton <$> do
                    A.char '\\' <|> A.char '"'
                quote = "\"" <$ A.string "\"\""


encoder :: forall t. HTable t => Encoders.Composite (t Result)
encoder = getConst $ htabulateA @t \field ->
  case hfield hspecs field of
    Spec {nullity, info} -> Const $
      runIdentity . (`hfield` field) >$<
        case nullity of
          Null -> Encoders.field $ Encoders.nullable build
          NotNull -> Encoders.field $ Encoders.nonNullable build
        where
          build = Encoder.binary (encode info)


builder :: HTable t => t Result -> Builder
builder input = buildRow $ getConst $ htabulateA \field ->
  Const $ pure $
    case hfield input field of
      Identity a ->
        case hfield hspecs field of
          Spec {nullity, info} -> case nullity of
            Null -> build <$> a
            NotNull -> Just $ build a
            where
              build =
                toStrict . toLazyByteString . Encoder.text (encode info)


buildRow :: [Maybe ByteString] -> Builder
buildRow elements =
  B.char8 '(' <>
  interfoldMap (B.char8 ',') (foldMap element) elements <>
  B.char8 ')'
  where
    element a
        | BS.null a = "\"\""
        | BS.all (A.notInClass ",\\\"()") a = B.byteString a
        | otherwise =
            B.char8 '"' <> BS.foldr ((<>) . escape) mempty a <> B.char8 '"'
        where
          escape = \case
            '"' -> B.string7 "\"\""
            '\\' -> B.string7 "\\\\"
            c -> B.char8 c


quoter :: HTable t => t Expr -> Opaleye.PrimExpr
quoter = Opaleye.FunExpr "ROW" . hfoldMap (pure . toPrimExpr)
