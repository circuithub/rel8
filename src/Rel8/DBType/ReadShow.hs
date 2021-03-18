module Rel8.DBType.ReadShow ( ReadShow(..) ) where

-- base
import Text.Read ( readEither )

-- rel8
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType ( parseDatabaseType )

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
--   deriving DBType via ReadShow Color
-- :}
-- 
-- will allow you to store @Color@ values in a single SQL column (stored as
-- @text@).
newtype ReadShow a = ReadShow { fromReadShow :: a }


-- | The 'DBType' instance for 'ReadShow' allows you to serialize a type using
-- Haskell's 'Read' and 'Show' instances:
--
-- @
-- data Color = Red | Green | Blue
--   deriving (Read, Show)
--   deriving DBType via ReadShow Color
-- @
instance (Read a, Show a) => DBType (ReadShow a) where
  typeInformation = parseDatabaseType parser printer typeInformation
    where
      parser = fmap ReadShow . readEither . Text.unpack
      printer = Text.pack . show . fromReadShow
