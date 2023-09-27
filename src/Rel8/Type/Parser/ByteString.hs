{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Rel8.Type.Parser.ByteString
  ( bytestring
  )
where

-- attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as A

-- base
import Control.Applicative ((<|>), many)
import Control.Monad (guard)
import Data.Bits ((.|.), shiftL)
import Data.Char (isOctDigit)
import Data.Foldable (fold)
import Prelude

-- base16
import Data.ByteString.Base16 (decodeBase16Untyped)

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- text
import qualified Data.Text as Text


bytestring :: A.Parser ByteString
bytestring = hex <|> escape
  where
    hex = do
      digits <- "\\x" *> A.takeByteString
      either (fail . Text.unpack) pure $ decodeBase16Untyped digits
    escape = fold <$> many (escaped <|> unescaped)
      where
        unescaped = A.takeWhile1 (/= '\\')
        escaped = BS.singleton <$> (backslash <|> octal)
          where
            backslash = '\\' <$ "\\\\"
            octal = do
              a <- A.char '\\' *> digit
              b <- digit
              c <- digit
              let
                result = a `shiftL` 6 .|. b `shiftL` 3 .|. c
              guard $ result < 0o400
              pure $ toEnum result
              where
                digit = do
                  c <- A.satisfy isOctDigit
                  pure $ fromEnum c - fromEnum '0'
