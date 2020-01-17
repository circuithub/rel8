{-# language FunctionalDependencies #-}
{-# language UndecidableInstances #-}

module Rel8.FromRow ( FromRow(..) ) where


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class FromRow sql haskell | sql -> haskell, haskell -> sql


instance ( FromRow sqlA haskellA, FromRow sqlB haskellB ) => FromRow ( sqlA, sqlB ) ( haskellA, haskellB )
