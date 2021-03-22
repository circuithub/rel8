module Rel8.PrimitiveType.ReadShow ( ReadShow(..) ) where

-- base
import Text.Read ( readEither )

-- rel8
import Rel8.DatabaseType ( parseDatabaseType )
import Rel8.PrimitiveType ( PrimitiveType( typeInformation ) )

-- text
import qualified Data.Text as Text


-- | A deriving-via helper type for column types that store a Haskell value
-- using a Haskell's 'Read' and 'Show' type classes.
--
-- The declaration:
-- 
-- >>> :{
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving PrimitiveType via ReadShow Color
-- :}
-- 
-- will allow you to store @Color@ values in a single SQL column (stored as
-- @text@).
newtype ReadShow a = ReadShow { fromReadShow :: a }


-- | The 'PrimitiveType' instance for 'ReadShow' allows you to serialize a type using
-- Haskell's 'Read' and 'Show' instances:
--
-- @
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving PrimitiveType via ReadShow Color
-- @
instance (Read a, Show a) => PrimitiveType (ReadShow a) where
  typeInformation = parseDatabaseType parser printer typeInformation
    where
      parser = fmap ReadShow . readEither . Text.unpack
      printer = Text.pack . show . fromReadShow
