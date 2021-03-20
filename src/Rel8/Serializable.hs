{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Serializable ( ExprFor(..), Serializable, hasqlRowDecoder, lit ) where

-- 
import qualified Hasql.Decoders as Hasql

-- base
import Data.Functor.Identity ( Identity( Identity ), runIdentity )

-- rel8
import Rel8.Context ( Context )
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( litExprWith )
import Rel8.HTable ( HTable( htraverse, htabulate, hdbtype, hfield ) )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Info ( HasInfo, decodeWith )
import Rel8.Table ( Columns, Table, fromColumns )


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
class ExprFor expr haskell => Serializable expr haskell | expr -> haskell where


-- | @ExprFor expr haskell@ witnesses that @expr@ is the "expression
-- representation" of the Haskell type @haskell@. You can think of this as the
-- type obtained if you were to quote @haskell@ constants into a query. 
--
-- This type class exists to provide "backwards" type inference for
-- 'Serializable'. While the functional dependency on 'Serializable' shows that
-- for any @expr@ there is exactly one @haskell@ type that is returned when the
-- expression is @select@ed, this type class is less restrictive, allowing for
-- their to be multiple expression types. Usually this is not the case, but for
-- @Maybe a@, we may allow expressions to be either @MaybeTable a'@ (where
-- @ExprFor a' a@), or just @Expr (Maybe a)@ (if @a@ is a single column).
class Table Expr expr => ExprFor expr haskell where
  unpack :: haskell -> Columns expr (Context Identity)
  pack :: Columns expr (Context Identity) -> haskell


instance {-# OVERLAPPABLE #-} (HasInfo b, a ~ Expr b) => ExprFor a b where
  unpack = HIdentity . pure
  pack (HIdentity a) = runIdentity a


instance DBType a => ExprFor (Expr (Maybe a)) (Maybe a) where
  unpack = HIdentity . pure
  pack (HIdentity a) = runIdentity a


instance (a ~ (a1, a2), ExprFor a1 b1, ExprFor a2 b2) => ExprFor a (b1, b2) where
  unpack (a, b) = HPair (unpack @a1 a) (unpack @a2 b)
  pack (HPair a b) = (pack @a1 a, pack @a2 b)


instance (a ~ (a1, a2, a3), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3) => ExprFor a (b1, b2, b3) where
  unpack (a, b, c) = HPair (unpack @a1 a) (HPair (unpack @a2 b) (unpack @a3 c))
  pack (HPair a (HPair b c)) = (pack @a1 a, pack @a2 b, pack @a3 c)


instance (a ~ (a1, a2, a3, a4), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3, ExprFor a4 b4) => ExprFor a (b1, b2, b3, b4) where
  unpack (a, b, c, d) = HPair (HPair (unpack @a1 a) (unpack @a2 b)) (HPair (unpack @a3 c) (unpack @a4 d))
  pack (HPair (HPair a b) (HPair c d)) = (pack @a1 a, pack @a2 b, pack @a3 c, pack @a4 d)


instance (HTable t, a ~ t (Context Expr), identity ~ Context Identity) => ExprFor a (t identity) where
  unpack = id
  pack = id


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Context Expr, identity ~ Context Identity, HTable t) => Serializable (s expr) (t identity) where


instance (a ~ b, HasInfo b) => Serializable (Expr a) b where


instance (Serializable a1 b1, Serializable a2 b2) => Serializable (a1, a2) (b1, b2) where


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3) => Serializable (a1, a2, a3) (b1, b2, b3) where


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3, Serializable a4 b4) => Serializable (a1, a2, a3, a4) (b1, b2, b3, b4) where


lit :: forall exprs haskell. Serializable exprs haskell => haskell -> exprs
lit x = fromColumns $ htabulate \i ->
  litExprWith (hfield hdbtype i) $ runIdentity $ hfield unpacked i
  where
    unpacked = unpack @exprs x


hasqlRowDecoder :: forall exprs haskell. Serializable exprs haskell => Hasql.Row haskell
hasqlRowDecoder = pack @exprs <$> htraverse (fmap Identity) decoders
  where
    decoders :: Columns exprs (Context Hasql.Row)
    decoders = htabulate (decodeWith . hfield hdbtype)
