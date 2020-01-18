{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.SimpleConstraints ( Selects, IsTableIn, Promote, WFHigherKinded, ConstrainHigherKinded ) where

import Data.Functor.Identity
import Rel8.ColumnSchema
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.Nest
import Rel8.Rewrite
import Rel8.Top
import Rel8.ZipLeaves
import {-# source #-} Rel8.Query


class
  ( Rewrite ColumnSchema ( Expr m ) schema row
  , ZipLeaves row row ( Expr m ) ( Expr m )
  ) => Selects m schema row


instance
  {-# overlapping #-}
  ( Rewrite ColumnSchema ( Expr m ) schema row
  , ZipLeaves row row ( Expr m ) ( Expr m )
  ) => Selects m schema row


data Hidden ( a :: k )

instance
  ( Rewrite ColumnSchema ( Expr m ) ( Hidden () ) row
  , ZipLeaves row row ( Expr m ) ( Expr m )
  ) => Selects m ( Hidden () ) row


class
  ZipLeaves a a (Expr m) (Expr m)
  => a `IsTableIn` m


instance
  {-# overlapping #-}
  ZipLeaves a a (Expr m) (Expr m)
  => a `IsTableIn` m


instance
  ( ZipLeaves ( Hidden () ) ( Hidden () ) ( Expr m ) ( Expr m )
  ) => ( Hidden () ) `IsTableIn` m


class
  ( Rewrite ( Expr ( Nest m ) ) ( Expr m ) b a
  , Rewrite ( Expr m ) ( Expr ( Nest m ) ) a b
  , ZipLeaves b a ( Expr ( Nest m ) ) ( Expr m )
  ) => Promote m a b


instance
  {-# overlapping #-}
  ( Rewrite ( Expr ( Nest m ) ) ( Expr m ) b a
  , Rewrite ( Expr m ) ( Expr ( Nest m ) ) a b
  , ZipLeaves b a ( Expr ( Nest m ) ) ( Expr m )
  ) => Promote m a b


instance
  ( Rewrite ( Expr ( Nest m ) ) ( Expr m ) b ( Hidden () )
  , Rewrite ( Expr m ) ( Expr ( Nest m ) ) ( Hidden () ) b
  , ZipLeaves b ( Hidden () ) ( Expr ( Nest m ) ) ( Expr m )
  ) => Promote m ( Hidden () ) b


class
  ( HigherKinded t
  , ZipRecord t ( Expr Query ) ( Expr Query ) Top
  , ZipRecord t ( Expr Query ) Identity Top
  ) => WFHigherKinded t


instance
  {-# overlapping #-}
  ( HigherKinded t
  , ZipRecord t ( Expr Query ) ( Expr Query ) Top
  , ZipRecord t ( Expr Query ) Identity Top
  ) => WFHigherKinded t


class
  ( HigherKinded t
  , ZipRecord t ( Expr m ) ( Expr m ) c
  ) => ConstrainHigherKinded m c t


instance
  {-# overlapping #-}
  ( HigherKinded t
  , ZipRecord t ( Expr m ) ( Expr m ) c
  ) => ConstrainHigherKinded m c t


instance
  ( HigherKinded Hidden
  , ZipRecord Hidden ( Expr m ) ( Expr m ) c
  ) => ConstrainHigherKinded m c Hidden
