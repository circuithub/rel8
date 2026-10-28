{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DisambiguateRecordFields #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

-- | Uses of tag types that are wrong for 'Shape'. This module must NOT
-- compile, so it isn't part of the test suite. Check it with
--
-- > cd rel8 && cabal exec -- ghc -fno-code -itests tests/type-errors/ADTTag.hs
--
-- which must fail with the errors noted on each binding below. GHC prints an
-- identical error only once per module, so the three @partial...@ bindings
-- produce one error between them; comment out the others to see each. The
-- errors for 'lifted' and the @Maybe@ instance stop GHC before it reports the
-- rest, so comment those two out to see the others.
module ADTTag where

-- base
import GHC.Generics ( Generic )
import GHC.TypeLits ( AppendSymbol, Symbol )
import Prelude

-- rel8
import Rel8
  ( ADTWith, ADTTag (TagConstructor)
  , DBEnum (enumTypeName, enumValue), DBEq, DBType
  , Expr, Name, TableSchema (TableSchema)
  , nameADTWith
  )
import qualified Rel8

-- rel8 tests
import Rel8.ADTWith.Test ( Shape (Dot), ShapeKind )


-- | The default (identity) 'TagConstructor', but these constructor names
-- don't match 'Shape''s. The labels do, but labels play no part in the
-- pairing.
data ShapeKindMismatch = MismatchCircle | MismatchSquare | MismatchDot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (DBEq, ADTTag)
  deriving DBType via Rel8.Enum ShapeKindMismatch


instance DBEnum ShapeKindMismatch where
  enumTypeName = "shape_kind_mismatch"
  enumValue = \case
    MismatchCircle -> "Circle"
    MismatchSquare -> "Square"
    MismatchDot -> "Dot"


-- | No constructor for 'Dot'.
data ShapeKindPartial = PartialCircle | PartialSquare
  deriving stock (Eq, Show, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindPartial


instance DBEnum ShapeKindPartial where
  enumTypeName = "shape_kind_partial"


instance ADTTag ShapeKindPartial where
  type TagConstructor ShapeKindPartial name = AppendSymbol "Partial" name


-- | Pairs 'Circle' and 'Square' with the same tag, so rows of one would read
-- back as the other.
data ShapeKindDuplicate = DuplicateRound | DuplicateSquare | DuplicateDot
  deriving stock (Eq, Show, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindDuplicate


instance DBEnum ShapeKindDuplicate where
  enumTypeName = "shape_kind_duplicate"


type DuplicateConstructor :: Symbol -> Symbol
type family DuplicateConstructor name where
  DuplicateConstructor "Circle" = "DuplicateRound"
  DuplicateConstructor "Square" = "DuplicateRound"
  DuplicateConstructor "Dot" = "DuplicateDot"


instance ADTTag ShapeKindDuplicate where
  type TagConstructor ShapeKindDuplicate name = DuplicateConstructor name


-- | A tag type with a non-nullary constructor.
data ShapeKindFat = FatCircle Int | FatSquare | FatDot
  deriving stock (Eq, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindFat


instance DBEnum ShapeKindFat where
  enumTypeName = "shape_kind_fat"
  enumValue = \case
    FatCircle _ -> "circle"
    FatSquare -> "square"
    FatDot -> "dot"


instance ADTTag ShapeKindFat where
  type TagConstructor ShapeKindFat name = AppendSymbol "Fat" name


-- | One error per constructor of 'Shape':
--
-- > `TagConstructor` names `Circle` as the tag of the constructor `Circle` of `Shape`,
-- > but the ADT tag type `ShapeKindMismatch` has no constructor `Circle`.
--
-- (and likewise for @Square@ and @Dot@).
mismatch :: String
mismatch =
  Rel8.showQuery $
    Rel8.values @(ADTWith ShapeKindMismatch Shape Expr) [Rel8.lit Dot]


-- | Rejected even though only @Circle@ is built:
--
-- > The ADT tag type `ShapeKindPartial` has 2 constructors, but the ADT `Shape` has 3;
-- > a tag type needs exactly one constructor per ADT constructor.
partial :: ADTWith ShapeKindPartial Shape Expr
partial = Rel8.buildADTWith @ShapeKindPartial @Shape @"Circle" (Rel8.lit 1)


-- | The same error as 'partial', for 'Rel8.constructADTWith'.
partialConstructed :: ADTWith ShapeKindPartial Shape Expr
partialConstructed =
  Rel8.constructADTWith @ShapeKindPartial @Shape \circle _square _dot -> circle 1


-- | The same error as 'partial', for a schema.
partialSchema :: TableSchema (ADTWith ShapeKindPartial Shape Name)
partialSchema =
  TableSchema
    { name = "shape_kind_partial_table"
    , columns = nameADTWith @ShapeKindPartial @Shape "kind" "radius" "side" "colour"
    }


-- | Two ADT constructors can't share a tag:
--
-- > `TagConstructor` names `DuplicateRound` as the tag of both `Circle` and `Square` of `Shape`;
-- > each constructor of an ADT needs its own constructor of the tag type `ShapeKindDuplicate`.
duplicate :: String
duplicate =
  Rel8.showQuery $
    Rel8.values @(ADTWith ShapeKindDuplicate Shape Expr) [Rel8.lit Dot]


-- | > The ADT tag type `ShapeKindFat` has a constructor `FatCircle` with fields;
-- > the constructors of a tag type must all be nullary.
fat :: ADTWith ShapeKindFat Shape Expr
fat = Rel8.buildADTWith @ShapeKindFat @Shape @"Dot"


-- | A tag type with a constructor that no constructor of 'Shape' is paired
-- with. A row holding it couldn't be read back, so it's rejected:
--
-- > The ADT tag type `ShapeKindExtra` has 4 constructors, but the ADT `Shape` has 3;
-- > a tag type needs exactly one constructor per ADT constructor.
data ShapeKindExtra = ExtraCircle | ExtraSquare | ExtraDot | ExtraUnused
  deriving stock (Eq, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindExtra


instance DBEnum ShapeKindExtra where
  enumTypeName = "shape_kind_extra"
  enumValue = \case
    ExtraCircle -> "circle"
    ExtraSquare -> "square"
    ExtraDot -> "dot"
    ExtraUnused -> "unused"


instance ADTTag ShapeKindExtra where
  type TagConstructor ShapeKindExtra name = AppendSymbol "Extra" name


extra :: ADTWith ShapeKindExtra Shape Expr
extra = Rel8.buildADTWith @ShapeKindExtra @Shape @"Dot"


-- | A misspelled constructor is reported as such, not as a missing tag
-- constructor @KindCirle@:
--
-- > The type `Shape` has no constructor `Cirle`.
misspelled :: ADTWith ShapeKind Shape Expr
misspelled = Rel8.buildADTWith @ShapeKind @Shape @"Cirle" (Rel8.lit 1)


-- | The tag type must be known where a value is lifted. Here GHC picks 'ADT'
-- for @r@ before seeing the signature:
--
-- > Couldn't match type ‘Tag’ with ‘ShapeKind’
--
-- @Rel8.lit \@(ADTWith ShapeKind Shape Expr) Dot@ works.
lifted :: ADTWith ShapeKind Shape Expr
lifted = let r = Rel8.lit Dot in r


-- | A tag column can't be null:
--
-- > Couldn't match type ‘True’ with ‘False’
-- > In the instance declaration for ‘ADTTag (Maybe ShapeKind)’
instance ADTTag (Maybe ShapeKind)
