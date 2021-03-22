{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Serializable ( ExprFor(..), Serializable, hasqlRowDecoder, lit ) where

-- base
import Data.Functor.Identity ( Identity )

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Context ( Column( I, unI, ColumnDecoder ), fromColumnDecoder )
import Rel8.DBType ( Column( fromInfoColumn ), DBType, decodeWith )
import Rel8.Expr ( Expr )
import Rel8.Expr.Instances ( Column( ExprColumn ) )
import Rel8.Expr.Opaleye ( litExprWith )
import Rel8.HTable ( HTable, hdbtype, hfield, htabulateMeta, htraverseMeta )
import Rel8.HTable.HIdentity ( HIdentity( HIdentity ) )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.PrimitiveType ( PrimitiveType )
import Rel8.Table ( Columns, Table, fromColumns )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative( WrapApplicative, unwrapApplicative ) )


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
  unpack :: haskell -> Columns expr (Column Identity)
  pack :: Columns expr (Column Identity) -> haskell


instance {-# OVERLAPPABLE #-} (DBType b, a ~ Expr b) => ExprFor a b where
  unpack = HIdentity . I
  pack (HIdentity a) = unI a


instance PrimitiveType a => ExprFor (Expr (Maybe a)) (Maybe a) where
  unpack = HIdentity . I
  pack (HIdentity a) = unI a


instance (a ~ (a1, a2), ExprFor a1 b1, ExprFor a2 b2) => ExprFor a (b1, b2) where
  unpack (a, b) = HPair (unpack @a1 a) (unpack @a2 b)
  pack (HPair a b) = (pack @a1 a, pack @a2 b)


instance (a ~ (a1, a2, a3), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3) => ExprFor a (b1, b2, b3) where
  unpack (a, b, c) = HPair (unpack @a1 a) (HPair (unpack @a2 b) (unpack @a3 c))
  pack (HPair a (HPair b c)) = (pack @a1 a, pack @a2 b, pack @a3 c)


instance (a ~ (a1, a2, a3, a4), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3, ExprFor a4 b4) => ExprFor a (b1, b2, b3, b4) where
  unpack (a, b, c, d) = HPair (HPair (unpack @a1 a) (unpack @a2 b)) (HPair (unpack @a3 c) (unpack @a4 d))
  pack (HPair (HPair a b) (HPair c d)) = (pack @a1 a, pack @a2 b, pack @a3 c, pack @a4 d)


instance (HTable t, a ~ t (Column Expr), identity ~ Column Identity) => ExprFor a (t identity) where
  unpack = id
  pack = id


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Column Expr, identity ~ Column Identity, HTable t) => Serializable (s expr) (t identity) where


instance (a ~ b, DBType b) => Serializable (Expr a) b where


instance (Serializable a1 b1, Serializable a2 b2) => Serializable (a1, a2) (b1, b2) where


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3) => Serializable (a1, a2, a3) (b1, b2, b3) where


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3, Serializable a4 b4) => Serializable (a1, a2, a3, a4) (b1, b2, b3, b4) where


lit :: forall exprs haskell. Serializable exprs haskell => haskell -> exprs
lit x = fromColumns $ htabulateMeta \i ->
  ExprColumn $ litExprWith (fromInfoColumn (hfield hdbtype i)) $ unI $ hfield unpacked i
  where
    unpacked = unpack @exprs x


hasqlRowDecoder :: forall exprs haskell. Serializable exprs haskell => Hasql.Row haskell
hasqlRowDecoder = unwrapApplicative $ pack @exprs <$> htraverseMeta (WrapApplicative . fmap I . fromColumnDecoder) decoders
  where
    decoders :: Columns exprs (Column Hasql.Row)
    decoders = htabulateMeta (ColumnDecoder . decodeWith . fromInfoColumn . hfield hdbtype)
