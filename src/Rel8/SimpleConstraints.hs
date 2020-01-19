{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.SimpleConstraints where

import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.Nest
import Rel8.Table


-- | @Selects m schema row@ says that in the monad @m@, the schema definition
-- @schema@ can be @SELECT@ed into the Haskell type @row@.
class
  ( Context row ~ Expr m
  , Context schema ~ ColumnSchema
  , Table schema
  , Table row
  , Compatible row ( Expr m ) row ( Expr m )
  , Compatible row ( Expr m ) schema ColumnSchema
  ) => Selects m schema row


instance
  {-# overlapping #-}
  ( Context row ~ Expr m
  , Context schema ~ ColumnSchema
  , Table schema
  , Table row
  , Compatible row ( Expr m ) row ( Expr m )
  , Compatible row ( Expr m ) schema ColumnSchema
  ) => Selects m schema row


data Hidden ( a :: k )


instance
  ( Context row ~ Expr m
  , Context ( Hidden () ) ~ ColumnSchema
  , Table ( Hidden () )
  , Table row
  , Compatible row ( Expr m ) row ( Expr m )
  , Compatible row ( Expr m ) ( Hidden () ) ColumnSchema
  ) => Selects m ( Hidden () ) row



-- | Makes sure that a given table (@a@) contains expressions compatible with
-- the monad @m@. This type class is essentially a scoping check.
class
  ( Compatible a ( Expr m ) a ( Expr m )
  , Context a ~ Expr m
  ) => a `IsTableIn` m


-- | @Promote m a b@ witnesses that the types @a@ and @b@ are morally the same,
-- but exist at different levels of scope. In particular, @Promote m a b@ says
-- @b@ is the same expression as @a@, where the scope has been increased by one.
class
  ( Compatible a ( Expr m ) b ( Expr ( Nest m ) )
  , Context a ~ Expr m
  , Context b ~ Expr ( Nest m )
  ) => Promote m a b where


instance
  {-# overlapping #-}
  ( Compatible a ( Expr m ) b ( Expr ( Nest m ) )
  , Context a ~ Expr m
  , Context b ~ Expr ( Nest m )
  ) => Promote m a b where


instance
  ( Compatible ( Hidden () ) ( Expr m ) b ( Expr ( Nest m ) )
  , Context ( Hidden () ) ~ Expr m
  , Context b ~ Expr ( Nest m )
  ) => Promote m ( Hidden () ) b where


-- | The sub-class of higher-kinded data types where all columns satisfy
-- an extra constraint. For example, @ConstrainHigherKinded m DBEq t@ is
-- the class of higher-kinded data types where all columns can be compared
-- for equality.
class
  ( HigherKindedTable t
  , HConstrainTraverse t ( Expr m ) c
  ) => ConstrainHigherKinded m c t
