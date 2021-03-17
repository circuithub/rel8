{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Rel8.DBFunctor ( DBFunctor(..) ) where

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName, decoder ), parseDatabaseType )
import Rel8.DatabaseType.Decoder ( listDecoder )


class DBFunctor f where
  liftDatabaseType :: DatabaseType a -> DatabaseType (f a)


instance DBFunctor [] where
  liftDatabaseType DatabaseType{ encode, typeName, decoder } = DatabaseType
    { encode = Opaleye.FunExpr "row" . pure . Opaleye.CastExpr (typeName <> "[]") . Opaleye.ArrayExpr . map encode
    , decoder = listDecoder decoder
    , typeName = "record"
    }


instance DBFunctor NonEmpty where
  liftDatabaseType = parseDatabaseType nonEmptyEither toList . liftDatabaseType
    where
      nonEmptyEither =
        maybe (Left "DBType.NonEmpty.decode: empty list") Right . nonEmpty
