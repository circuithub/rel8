{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Column
  ( Column(..)
  , ColumnDecoder
  , fromField
  , readDecoder
  , columnDecoderToRowParser
  , maybeColumnDecoder
  , ColumnEncoder
  , toField
  , showEncoder
  , nullEncoder
  , runColumnEncoder
  , case_
  , fromJust
  , just
  , lit
  , selectColumn
  , toOpaleyeColumn
  , traversePrimExpr
  , write
  , zipColumnsM
  , (==.)
  , (<=.)
  , (<.)
  , (>=.)
  , (>.)
  ) where

import Control.Applicative ( Const(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )
import Data.Binary.Builder ( toLazyByteString )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy.Char8
import Data.Coerce ( coerce )
import Data.Functor.Contravariant ( Op(..) )
import Database.PostgreSQL.Simple.FromField ( Conversion, Field, FromField )
import qualified Database.PostgreSQL.Simple.FromField as PG
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Database.PostgreSQL.Simple.ToField ( Action(..) )
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


newtype Column a =
  Column { toPrimExpr :: Opaleye.PrimExpr }


selectColumn :: Const String a -> Column a
selectColumn = Column . Opaleye.BaseTableAttrExpr . getConst


traversePrimExpr
  :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Column a -> f (Column a)
traversePrimExpr f (Column x) =
  Column <$> f x


zipColumnsM
  :: Applicative m
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> m Opaleye.PrimExpr)
  -> Column a -> Column a -> m (Column a)
zipColumnsM f (Column x) (Column y) = fmap Column $ f x y


toOpaleyeColumn :: Column a -> Opaleye.Column b
toOpaleyeColumn = coerce


fromJust :: Column (Maybe a) -> Column a
fromJust = coerce


case_ :: [(Column Bool, Column a)] -> Column a -> Column a
case_ = coerce Opaleye.CaseExpr


lit :: Opaleye.Literal -> Column a
lit = coerce Opaleye.ConstExpr


write :: Functor f => ((f Opaleye.PrimExpr, String) -> m ()) -> f (Column a) -> Const String a -> m ()
write f values (Const columnName) = f (fmap toPrimExpr values, columnName)


just :: Column a -> Column (Maybe a)
just = coerce


newtype ColumnDecoder a =
  ColumnDecoder (ReaderT Field (ReaderT (Maybe ByteString) Conversion) a)


fromField :: forall a. FromField a => ColumnDecoder a
fromField = coerce $ PG.fromField @a


-- TODO Error handling
readDecoder :: forall a. Read a => ColumnDecoder a
readDecoder = coerce $ \x -> fmap (read @a) . PG.fromField @String x


columnDecoderToRowParser :: ColumnDecoder a -> RowParser a
columnDecoderToRowParser = coerce fieldWith


maybeColumnDecoder :: ColumnDecoder a -> ColumnDecoder (Maybe a)
maybeColumnDecoder (ColumnDecoder parser) =
  ColumnDecoder $ ReaderT \field ->
  ReaderT $
  maybe
    (pure Nothing)
    (runReaderT (runReaderT (Just <$> parser) field) . Just)


newtype ColumnEncoder a =
  ColumnEncoder (Op Opaleye.PrimExpr a)


toField :: PG.ToField a => ColumnEncoder a
toField =
  ColumnEncoder $ Op \a -> case PG.toField a of
    Plain builder ->
      Opaleye.ConstExpr $
      Opaleye.OtherLit $
      Data.ByteString.Lazy.Char8.unpack $
      toLazyByteString builder


showEncoder :: forall a. Show a => ColumnEncoder a
showEncoder = coerce (O.ConstExpr . O.StringLit . show @a)


nullEncoder :: ColumnEncoder a -> ColumnEncoder (Maybe a)
nullEncoder =
  ColumnEncoder . Op . maybe (Opaleye.ConstExpr Opaleye.NullLit) . coerce


runColumnEncoder :: ColumnEncoder a -> a -> Column a
runColumnEncoder f = coerce f


(==.) :: Column a -> Column a -> Column Bool
(==.) = coerce (Opaleye.BinExpr (Opaleye.:==))


(<=.) :: Column a -> Column a -> Column Bool
(<=.) = coerce (Opaleye.BinExpr (Opaleye.:<=))


(<.) :: Column a -> Column a -> Column Bool
(<.) = coerce (Opaleye.BinExpr (Opaleye.:<))


(>=.) :: Column a -> Column a -> Column Bool
(>=.) = coerce (Opaleye.BinExpr (Opaleye.:>=))


(>.) :: Column a -> Column a -> Column Bool
(>.) = coerce (Opaleye.BinExpr (Opaleye.:>))
