{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Tag
  ( EitherTag( IsLeft, IsRight ), isLeft, isRight
  , MaybeTag( IsJust )
  , Tag( Tag )
  )
where

-- base
import Data.Bool ( bool )
import Data.Kind ( Type )
import Data.Semigroup ( Min( Min ) )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( mapTypeInformation, parseTypeInformation )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Ord ( DBOrd )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )

-- text
import Data.Text ( Text )


type EitherTag :: Type
data EitherTag = IsLeft | IsRight
  deriving stock (Eq, Ord, Read, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via (Min EitherTag)
  deriving anyclass (DBEq, DBOrd)


instance DBType EitherTag where
  typeInformation = mapTypeInformation to from typeInformation
    where
      to = bool IsLeft IsRight
      from IsLeft = False
      from IsRight = True


instance DBSemigroup EitherTag where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpAnd)


instance DBMonoid EitherTag where
  memptyExpr = litExpr mempty


isLeft :: Expr EitherTag -> Expr Bool
isLeft = (litExpr IsLeft ==.)


isRight :: Expr EitherTag -> Expr Bool
isRight = (litExpr IsRight ==.)


type MaybeTag :: Type
data MaybeTag = IsJust
  deriving stock (Eq, Ord, Read, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via (Min MaybeTag)
  deriving anyclass (DBEq, DBOrd)


instance DBType MaybeTag where
  typeInformation = parseTypeInformation to from typeInformation
    where
      to False = Left "MaybeTag can't be false"
      to True = Right IsJust
      from _ = True


instance DBSemigroup MaybeTag where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr Opaleye.OpAnd)


instance DBMonoid MaybeTag where
  memptyExpr = litExpr mempty


newtype Tag = Tag Text
  deriving newtype
    ( Eq, Ord, Read, Show
    , DBType, DBEq, DBOrd
    )
