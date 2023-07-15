{-# language RecordWildCards #-}
{-# language StrictData #-}

module Rel8.Type.Name
  ( TypeName (..)
  , showTypeName
  )
where

-- base
import Data.Semigroup (mtimesDefault)
import Data.String (IsString, fromString)
import Prelude

-- pretty
import Text.PrettyPrint (Doc, comma, hcat, parens, punctuate, text)

-- rel8
import Rel8.Schema.QualifiedName (QualifiedName, ppQualifiedName)


-- | A PostgreSQL type consists of a 'QualifiedName' (name, schema), and
-- optional 'modifiers' and 'arrayDepth'. 'modifiers' will usually be @[]@,
-- but a type like @numeric(6, 2)@ will have @["6", "2"]@. 'arrayDepth' is
-- always @0@ for non-array types.
data TypeName = TypeName
  { name :: QualifiedName
    -- ^ The name (and schema) of the type.
  , modifiers :: [String]
    -- ^ Any modifiers applied to the underlying type.
  , arrayDepth :: Word
    -- ^ If this is an array type, the depth of that array (@1@ for @[]@, @2@
    -- for @[][]@, etc).
  }


-- | Constructs 'TypeName's with 'schema' set to 'Nothing', 'modifiers' set
-- to @[]@ and 'arrayDepth' set to @0@.
instance IsString TypeName where
  fromString string =
    TypeName
      { name = fromString string
      , modifiers = []
      , arrayDepth = 0
      }


ppTypeName :: TypeName -> Doc
ppTypeName TypeName {..} =
  ppQualifiedName name <> modifier <> mtimesDefault arrayDepth (text "[]")
  where
    modifier
      | null modifiers = mempty
      | otherwise = parens (hcat $ punctuate comma $ text <$> modifiers)


showTypeName :: TypeName -> String
showTypeName = show . ppTypeName
