{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Rel8.Type.ReadShow (ReadShow (..)) where

-- base
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Prelude
import Text.Read (readMaybe)

-- rel8
import Rel8.Type (DBType (typeInformation))
import Rel8.Type.Information (parseTypeInformation)

-- text
import qualified Data.Text as Text


{- | A deriving-via helper type for column types that store a Haskell value
using a Haskell's 'Read' and 'Show' type classes.
-}
type ReadShow :: Type -> Type
newtype ReadShow a = ReadShow {fromReadShow :: a}


instance (Read a, Show a, Typeable a) => DBType (ReadShow a) where
  typeInformation = parseTypeInformation parser printer typeInformation
    where
      parser (Text.unpack -> t) = case readMaybe t of
        Just ok -> Right $ ReadShow ok
        Nothing -> Left $ "Could not read " <> t <> " as a " <> show (typeRep (Proxy @a))
      printer = Text.pack . show . fromReadShow
