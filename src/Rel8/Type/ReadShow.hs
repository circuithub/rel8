module Rel8.Type.ReadShow ( ReadShow(..) ) where

-- base
import Prelude 
import Text.Read ( readEither )

-- rel8
import Rel8.Type ( DBType( typeInformation ) )
import Rel8.Type.Information ( parseTypeInformation )

-- text
import qualified Data.Text as Text


-- | A deriving-via helper type for column types that store a Haskell value
-- using a Haskell's 'Read' and 'Show' type classes.
newtype ReadShow a = ReadShow { fromReadShow :: a }


instance (Read a, Show a) => DBType (ReadShow a) where
  typeInformation = parseTypeInformation parser printer typeInformation
    where
      parser = fmap ReadShow . readEither . Text.unpack
      printer = Text.pack . show . fromReadShow
