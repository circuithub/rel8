{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Rel8.DBFunctor ( DBFunctor(..) ) where

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName, decoder ), parseDatabaseType )
import Rel8.DatabaseType.Decoder ( HasqlDecoder( DecodeNull, DecodeNotNull ) )


class DBFunctor f where
  liftDatabaseType :: DatabaseType a -> DatabaseType (f a)


instance DBFunctor [] where
  liftDatabaseType DatabaseType{ encode, typeName, decoder } = DatabaseType
    { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode
    , decoder = case decoder of
        DecodeNotNull v f ->
          DecodeNotNull (Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nonNullable (f <$> v)) id

        DecodeNull v f -> DecodeNull v' \case
          Nothing -> pure <$> f Nothing
          Just xs -> traverse f xs
          where
            v' = Hasql.composite $ Hasql.field $ Hasql.nonNullable $ Hasql.listArray $ Hasql.nullable v
    , typeName = "record"
    }


instance DBFunctor NonEmpty where
  liftDatabaseType = parseDatabaseType nonEmptyEither toList . liftDatabaseType
    where
      nonEmptyEither =
        maybe (Left "DBType.NonEmpty.decode: empty list") Right . nonEmpty
