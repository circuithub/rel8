{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingVia #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Tag
  ( EitherTag( IsLeft, IsRight ), isLeft, isRight
  , MaybeTag( IsJust )
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
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Opaleye ( litPrimExpr )
import Rel8.Kind.Nullability ( Nullability( NonNullable ) )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information ( mapTypeInformation, parseTypeInformation )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Ord ( DBOrd )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


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
  Expr a <>. Expr b = Expr (Opaleye.BinExpr Opaleye.OpAnd a b)


instance DBMonoid EitherTag where
  memptyExpr = litPrimExpr mempty


isLeft :: Expr 'NonNullable EitherTag -> Expr 'NonNullable Bool
isLeft = (litPrimExpr IsLeft ==.)


isRight :: Expr 'NonNullable EitherTag -> Expr 'NonNullable Bool
isRight = (litPrimExpr IsLeft ==.)


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
  Expr a <>. Expr b = Expr (Opaleye.BinExpr Opaleye.OpAnd a b)


instance DBMonoid MaybeTag where
  memptyExpr = litPrimExpr mempty
