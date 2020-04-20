{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

-- | This module exposes functionality for working with individual columns in
-- SQL rows. It is somewhat untyped, as the type obligations are dealt with by
-- rows.

module Rel8.Column
  ( Column( Column )
  , toPrimExpr
  , toOpaleyeColumn
  , traversePrimExpr
  , zipColumnsM

    -- * Decoding
  , ColumnDecoder
  , columnDecoderToRowParser
  , fromField
  , maybeColumnDecoder
  , readDecoder

    -- * Encoding
  , ColumnEncoder
  , toField
  , showEncoder
  , nullEncoder
  , runColumnEncoder

    -- * Combinators
  , case_
  , fromJust
  , just
  , lit
  , (==.)
  , (<=.)
  , (<.)
  , (>=.)
  , (>.)
  , (&&.)
  , (||.)
  , not_
  , ColumnSchema( ColumnSchema )
  , select
  , write
  , concreteColumn
  , derivedColumn
  , isNull
  , jsonEncoder
  , jsonDecoder
  ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, encode )

-- base
import Data.Coerce ( coerce )
import Data.Typeable ( Typeable )

-- binary
import Data.Binary.Builder ( toLazyByteString )

-- bytestring
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 ( unpack )
import qualified Data.ByteString.Lazy.Char8 as Lazy

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import Opaleye.Internal.HaskellDB.PrimQuery
  ( BinOp( (:==), (:<=), (:<), (:>=), (:>), (:&&), (:||) )
  , Literal( OtherLit, StringLit, ByteStringLit, NullLit )
  , PrimExpr( ConstExpr, CaseExpr, BinExpr, UnExpr, BaseTableAttrExpr )
  , UnOp( OpNot, OpIsNull )
  )
import Opaleye.Internal.PackMap ( PackMap( PackMap ) )
import Opaleye.Internal.Table ( Writer( Writer ) )

-- postgresql-simple
import Database.PostgreSQL.Simple.FromField ( Conversion, Field, FromField )
import qualified Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Database.PostgreSQL.Simple.ToField
  ( Action( Plain, Escape, EscapeByteA, EscapeIdentifier )
  , ToField
  )
import qualified Database.PostgreSQL.Simple.ToField as PG ( toField )


-- | A @Column@ is a wrapper around a single column SQL expression.
newtype Column a =
  Column { toPrimExpr :: PrimExpr }


-- | Traverse a @Column@ into its primitive 'PrimExpr'.
traversePrimExpr
  :: Functor f => (PrimExpr -> f PrimExpr) -> Column a -> f (Column a)
traversePrimExpr f (Column x) =
  Column <$> f x


-- | Effectfully combine two 'Column's into one.
zipColumnsM
  :: Applicative m
  => (PrimExpr -> PrimExpr -> m PrimExpr)
  -> Column a
  -> Column a
  -> m (Column a)
zipColumnsM f (Column x) (Column y) =
  Column <$> f x y


-- | Convert a Rel8 'Column' into an Opaleye 'Column'.
toOpaleyeColumn :: Column a -> Opaleye.Column b
toOpaleyeColumn =
  coerce


-- | Reinterpret a 'Column' as one without @null@.
fromJust :: Column (Maybe a) -> Column a
fromJust =
  coerce


-- | Construct a @CASE@ expression.
case_ :: [(Column Bool, Column a)] -> Column a -> Column a
case_ =
  coerce CaseExpr


-- | Construct a column from a primitive Opaleye literal.
lit :: Literal -> Column a
lit =
  coerce ConstExpr


-- | Lift a column into a one that can also contain @null@.
just :: Column a -> Column (Maybe a)
just =
  coerce


-- | A @ColumnDecoder@ decodes results returned from a PostgreSQL server.
newtype ColumnDecoder a =
  ColumnDecoder (Field -> Maybe ByteString -> Conversion a)


-- | Construct a @ColumnDecoder@ by using a @postgresql-simple@ 'FromField'
-- instance.
fromField :: forall a. FromField a => ColumnDecoder a
fromField =
  coerce $ PG.fromField @a


-- TODO Error handling
-- | Construct a @ColumnDecoder@ by interpreting strings as a serialisation
-- under Haskell's 'Read' type class.
--
-- This @ColumnDecoder@ requires the type of the column being decoded to be
-- compatible with @postgresql-simple@'s 'FromField' 'String' instance.
readDecoder :: forall a. Read a => ColumnDecoder a
readDecoder =
  coerce \x -> fmap (read @a) . PG.fromField @String x


-- | Convert a 'ColumnDecoder' into a 'RowParser'.
columnDecoderToRowParser :: ColumnDecoder a -> RowParser a
columnDecoderToRowParser =
  coerce fieldWith


-- | Convert a 'ColumnDecoder' into one that can also parse @null@ into
-- 'Nothing'.
maybeColumnDecoder :: ColumnDecoder a -> ColumnDecoder (Maybe a)
maybeColumnDecoder (ColumnDecoder parser) =
  ColumnDecoder \field ->
    maybe (pure Nothing) (fmap Just . parser field . Just)


-- | A @ColumnEncoder@ encodes Haskell values into SQL literals.
newtype ColumnEncoder a =
  ColumnEncoder { runColumnEncoder :: a -> Column a }


-- | Create a 'ColumnEncoder' using @postgresql-simple@'s 'ToField' type class.
toField :: ToField a => ColumnEncoder a
toField =
  ColumnEncoder \a -> Column case PG.toField a of
    Plain builder ->
      ConstExpr $ OtherLit $ Lazy.unpack $ toLazyByteString builder

    Escape bs ->
      ConstExpr $ StringLit $ unpack bs

    EscapeByteA bs ->
      ConstExpr $ ByteStringLit bs

    EscapeIdentifier i ->
      ConstExpr $ OtherLit $ "\"" <> unpack i <> "\""


-- | Create a 'ColumnEncoder' by encoding a Haskell value as a string, according
-- to its 'Show' instance.
showEncoder :: forall a. Show a => ColumnEncoder a
showEncoder =
  coerce (ConstExpr . StringLit . show @a)


-- | Create a 'ColumnEncoder' by encoding a Haskell value as JSON, according to
-- its 'ToJSON' instance.
jsonEncoder :: forall a. ToJSON a => ColumnEncoder a
jsonEncoder =
  coerce (ConstExpr . StringLit . Lazy.unpack . encode @a)


-- | Create a 'ColumnDecoder' by decoding a SQL column as JSON, and then parsing
-- that JSON into a Haskell value according to its 'FromJSON' instance.
jsonDecoder :: forall a. (FromJSON a, Typeable a) => ColumnDecoder a
jsonDecoder =
  coerce (PG.fromJSONField @a)


-- | Lift a 'ColumnEncoder' to also be able to encode 'Nothing' as @null@.
nullEncoder :: ColumnEncoder a -> ColumnEncoder (Maybe a)
nullEncoder =
  ColumnEncoder . maybe (Column (ConstExpr NullLit)) . coerce


-- | Compare two columns for equality using the SQL @=@ operator.
(==.) :: Column a -> Column a -> Column Bool
(==.) =
  coerce (BinExpr (:==))


-- | Compare two columns for order using the SQL @<=@ operator.
(<=.) :: Column a -> Column a -> Column Bool
(<=.) =
  coerce (BinExpr (:<=))


-- | Compare two columns for order using the SQL @<@ operator.
(<.) :: Column a -> Column a -> Column Bool
(<.) =
  coerce (BinExpr (:<))


-- | Compare two columns for order using the SQL @>=@ operator.
(>=.) :: Column a -> Column a -> Column Bool
(>=.) =
  coerce (BinExpr (:>=))


-- | Compare two columns for order using the SQL @>@ operator.
(>.) :: Column a -> Column a -> Column Bool
(>.) =
  coerce (BinExpr (:>))


-- | Combine two columns using the SQL @AND@ operator.
(&&.) :: Column a -> Column a -> Column Bool
(&&.) =
  coerce (BinExpr (:&&))


-- | Combine two columns using the SQL @OR@ operator.
(||.) :: Column Bool -> Column Bool -> Column Bool
(||.) =
  coerce (BinExpr (:||))


-- | Negate a column using the SQL @NOT@ operator.
not_ :: Column Bool -> Column Bool
not_ =
  coerce (UnExpr OpNot)


-- | A @ColumnSchema@ packs up how to project a column from a SQL table, and
-- how to write data back into the same SQL table.
data ColumnSchema a =
  ColumnSchema
    { select :: Column a
    , write :: Writer (Column a) ()
    }


-- | A concrete column is one that exists in a SQL table.
concreteColumn :: String -> ColumnSchema a
concreteColumn columnName =
  ColumnSchema
    { select =
      Column (BaseTableAttrExpr columnName)

    , write = Writer $ PackMap \f values ->
        f (fmap toPrimExpr values, columnName)
    }


-- | Derived columns are _not_ written to, they are derived from other columns.
--
-- These columns can be considered as views or virtual columns, and a useful
-- when writing a query, but are not stored.
derivedColumn
  :: (Column a -> Column b)
  -> String
  -> ColumnSchema b
derivedColumn f columnName =
  ColumnSchema
    { select = f (Column (BaseTableAttrExpr columnName))
    , write = pure ()
    }


-- | Check if a column is @null@, using the SQL @IS NULL@ operator.
isNull :: Column a -> Column Bool
isNull =
  coerce (UnExpr OpIsNull)
