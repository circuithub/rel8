{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
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
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), many, optional)
import Data.Foldable (fold)
import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind ( Constraint, Type )
import Data.List (uncons)
import Prelude

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( castExpr, fromPrimExpr, toPrimExpr )
import Rel8.Schema.HTable ( HTable, hfield, hspecs, htabulate, htabulateA )
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
import Rel8.Type.Decoder (Decoder (Decoder), Parser)
import qualified Rel8.Type.Decoder as Decoder
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


instance DBComposite a => DBType (Composite a) where
  typeInformation = TypeInformation
    { decode =
        Decoder
          { binary = Hasql.composite (Composite . fromResult @_ @(HKD a Expr) <$> decoder)
          , parser = fmap (Composite . fromResult @_ @(HKD a Expr)) . parser
          , delimiter = ','
          }
    , encode = encoder . litHTable . toResult @_ @(HKD a Expr) . unComposite
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
compose = castExpr . fromPrimExpr . encoder . toColumns


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


decoder :: HTable t => Hasql.Composite (t Result)
decoder = unwrapApplicative $ htabulateA \field ->
  case hfield hspecs field of
    Spec {nullity, info} -> WrapApplicative $ Identity <$>
      case nullity of
        Null -> Hasql.field $ Hasql.nullable $ Decoder.binary $ decode info
        NotNull -> Hasql.field $ Hasql.nonNullable $ Decoder.binary $ decode info


encoder :: HTable t => t Expr -> Opaleye.PrimExpr
encoder a = Opaleye.FunExpr "ROW" exprs
  where
    exprs = getConst $ htabulateA \field -> case hfield a field of
      expr -> Const [toPrimExpr expr]


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
          Null -> traverse (Decoder.parser (decode info)) mbytes
          NotNull -> case mbytes of
            Nothing -> Left "composite: unexpected null"
            Just bytes -> Decoder.parser (decode info) bytes
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
            contents = fold <$> many (unquote <|> unescape)
              where
                unquote = A.takeWhile1 (A.notInClass "\"\\")
                unescape = A.char '\\' *> do
                  BS.singleton <$> do
                    A.char '\\' <|> A.char '"'
