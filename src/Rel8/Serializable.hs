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

module Rel8.Serializable ( ExprFor, Serializable(..) ) where

-- base
import Control.Applicative ( Applicative( liftA2 ), liftA3 )
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.Functor.Identity ( Identity( Identity ) )

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Context ( Context )
import Rel8.DBType ( DBType( typeInformation ) )
import Rel8.DatabaseType ( DatabaseType( DatabaseType, encode, typeName, decoder ) )
import Rel8.DatabaseType.Decoder ( Decoder, runDecoder )
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( litExprWith )
import Rel8.HTable ( HTable( HField, htraverse, htabulate, hdbtype, hfield ) )
import Rel8.Table ( Table, fromColumns )


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
class ExprFor expr haskell => Serializable expr haskell | expr -> haskell where
  lit :: haskell -> expr

  rowParser :: forall f. (Applicative f, Traversable f)
    => (forall x. Decoder x -> Decoder (f x))
    -> Hasql.Row (f haskell)


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
class Table Expr expr => ExprFor expr haskell


instance {-# OVERLAPPABLE #-} (DBType b, a ~ Expr b)                                        => ExprFor a                b


instance DBType a                                                                           => ExprFor (Expr (Maybe a)) (Maybe a)


instance (a ~ (a1, a2), ExprFor a1 b1, ExprFor a2 b2)                                       => ExprFor a                (b1, b2)


instance (a ~ (a1, a2, a3), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3)                    => ExprFor a                (b1, b2, b3)


instance (a ~ (a1, a2, a3, a4), ExprFor a1 b1, ExprFor a2 b2, ExprFor a3 b3, ExprFor a4 b4) => ExprFor a                (b1, b2, b3, b4)


instance (HTable t, a ~ t (Context Expr), identity ~ Context Identity)                      => ExprFor a                (t identity)


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Context Expr, identity ~ Context Identity, HTable t) => Serializable (s expr) (t identity) where
  rowParser liftDecoder = getCompose $ htraverse (fmap pure) $ htabulate (f liftDecoder)
    where
      f :: forall f x. (forall y. Decoder y -> Decoder (f y)) -> HField t x -> Compose Hasql.Row f x
      f liftDecoder_ i = case hfield hdbtype i of
        databaseType -> Compose $ runDecoder $ liftDecoder_ $ decoder databaseType

  lit t =
    fromColumns $ htabulate \i ->
      case (hfield (hdbtype @t) i, hfield t i) of
        (databaseType, Identity x) -> litExprWith databaseType x


instance (DBType a, a ~ b) => Serializable (Expr a) b where
  rowParser liftDecoder =
    runDecoder (liftDecoder (decoder (typeInformation @a)))

  lit = Expr . Opaleye.CastExpr typeName . encode
    where
      DatabaseType{ encode, typeName } = typeInformation


instance (Serializable a1 b1, Serializable a2 b2) => Serializable (a1, a2) (b1, b2) where
  rowParser liftValue =
    liftA2 (liftA2 (,)) (rowParser @a1 liftValue) (rowParser @a2 liftValue)

  lit (a, b) = (lit a, lit b)


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3) => Serializable (a1, a2, a3) (b1, b2, b3) where
  rowParser liftValue =
    liftA3 (liftA3 (,,)) (rowParser @a1 liftValue) (rowParser @a2 liftValue) (rowParser @a3 liftValue)

  lit (a, b, c) = (lit a, lit b, lit c)


instance (Serializable a1 b1, Serializable a2 b2, Serializable a3 b3, Serializable a4 b4) => Serializable (a1, a2, a3, a4) (b1, b2, b3, b4) where
  rowParser liftValue =
    (\a b c d -> (,,,) <$> a <*> b <*> c <*> d) <$> rowParser @a1 liftValue <*> rowParser @a2 liftValue <*> rowParser @a3 liftValue <*> rowParser @a4 liftValue

  lit (a, b, c, d) = (lit a, lit b, lit c, lit d)

