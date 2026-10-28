{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.Type.Tag
  ( EitherTag( IsLeft, IsRight ), isLeft, isRight
  , MaybeTag( IsJust )
  , Tag( Tag )
  , ADTTag( TagConstructor )
  , ADTTagFor( tagValue ), isTag
  , HasConstructor
  )
where

-- base
import Data.Bool ( bool )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Semigroup ( Min( Min ) )
import Data.Type.Bool ( type (||) )
import GHC.Generics
  ( Generic, Rep
  , (:+:)( L1, R1 ), M1( M1 ), U1( U1 )
  , C, D, Meta( MetaCons )
  , to
  )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.Expr.Eq ( (==.) )
import Rel8.Internal.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Internal.Expr.Serialize ( litExpr )
import Rel8.Internal.Schema.Null ( NotNull )
import Rel8.Internal.Type.Eq ( DBEq )
import Rel8.Internal.Type ( DBType, typeInformation )
import Rel8.Internal.Type.Information ( mapTypeInformation, parseTypeInformation )
import Rel8.Internal.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Internal.Type.Ord ( DBOrd )
import Rel8.Internal.Type.Semigroup ( DBSemigroup, (<>.) )

-- text
import Data.Text ( Text, pack )


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


type Tag :: Type
newtype Tag = Tag Text
  deriving newtype
    ( Eq, Ord, Read, Show
    , DBType, DBEq, DBOrd
    )


type ADTTag :: Type -> Constraint
class (Eq tag, DBType tag, DBEq tag, NotNull tag) => ADTTag tag where
  type TagConstructor tag (name :: Symbol) :: Symbol
  type TagConstructor _tag name = name


type ADTTagFor :: Type -> Symbol -> Constraint
class ADTTag tag => ADTTagFor tag name where
  tagValue :: tag


isTag :: forall tag name. ADTTagFor tag name => tag -> Bool
isTag = (== tagValue @tag @name)


instance ADTTag Tag


instance KnownSymbol name => ADTTagFor Tag name where
  tagValue = Tag (pack (symbolVal (Proxy @name)))


instance {-# OVERLAPPABLE #-}
  ( ADTTag tag
  , Generic tag
  , GTagConstructor (TagConstructor tag name) (Rep tag)
  )
  => ADTTagFor tag name
 where
  tagValue = to (gtagConstructor @(TagConstructor tag name))


type GTagConstructor :: Symbol -> (Type -> Type) -> Constraint
class GTagConstructor target rep where
  gtagConstructor :: rep x


instance GTagConstructor target rep => GTagConstructor target (M1 D meta rep) where
  gtagConstructor = M1 (gtagConstructor @target)


instance GTagConstructorSum (HasConstructor target a) target a b =>
  GTagConstructor target (a :+: b)
 where
  gtagConstructor = gtagConstructorSum @(HasConstructor target a) @target


instance rep ~ U1 => GTagConstructor target (M1 C meta rep) where
  gtagConstructor = M1 U1


type GTagConstructorSum :: Bool -> Symbol -> (Type -> Type) -> (Type -> Type) -> Constraint
class GTagConstructorSum inLeft target a b where
  gtagConstructorSum :: (a :+: b) x


instance GTagConstructor target a => GTagConstructorSum 'True target a b where
  gtagConstructorSum = L1 (gtagConstructor @target)


instance GTagConstructor target b => GTagConstructorSum 'False target a b where
  gtagConstructorSum = R1 (gtagConstructor @target)


type HasConstructor :: Symbol -> (Type -> Type) -> Bool
type family HasConstructor name rep where
  HasConstructor name (M1 D _meta rep) = HasConstructor name rep
  HasConstructor name (a :+: b) = HasConstructor name a || HasConstructor name b
  HasConstructor name (M1 C ('MetaCons name _fixity _isRecord) _rep) = 'True
  HasConstructor _name _rep = 'False
