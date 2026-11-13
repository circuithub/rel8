{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

-- | Sum tables whose tag column is a PostgreSQL @enum@ rather than @text@.
module Rel8.ADTWith.Test
  ( Shape (..)
  , genShape
  , shapeSize
  , ShapeKind (..)
  , shapeKindTable
  , ShapeKindSnake (..)
  , shapeKindSnakeTable
  , ShapeKindShuffled (..)
  , shapeKindShuffledTable
  , Labelled (..)
  , genLabelled
  , labelledTable
  , createEnumTypes
  )
where

-- base
import Data.Int ( Int32 )
import GHC.Generics ( Generic )
import GHC.TypeLits ( AppendSymbol, Symbol )
import Prelude

-- hedgehog
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- rel8
import Rel8
  ( ADTWith, ADTTag (TagConstructor)
  , Column, DBEnum (enumTypeName, enumValue), DBEq, DBType
  , HADTWith, Name, Rel8able, Result, TableSchema (TableSchema)
  , nameADTWith
  )
import qualified Rel8

-- text
import Data.Text ( Text )


data Shape f
  = Circle (Column f Int32)
  | Square (Column f Int32) (Column f Text)
  | Dot
  deriving stock Generic


deriving stock instance f ~ Result => Show (Shape f)
deriving stock instance f ~ Result => Eq (Shape f)
deriving stock instance f ~ Result => Ord (Shape f)


genShape :: Hedgehog.MonadGen m => m (Shape Result)
genShape = Gen.choice
  [ Circle <$> Gen.integral (Range.linear 0 100)
  , Square <$> Gen.integral (Range.linear 0 100) <*> Gen.text (Range.linear 0 10) Gen.alpha
  , pure Dot
  ]


shapeSize :: Shape Result -> Int32
shapeSize = \case
  Circle r -> r
  Square s _ -> s * s
  Dot -> 0


data ShapeKind = KindCircle | KindSquare | KindDot
  deriving stock (Eq, Show, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKind


instance DBEnum ShapeKind where
  enumTypeName = "shape_kind"
  enumValue = \case
    KindCircle -> "round"
    KindSquare -> "boxy"
    KindDot -> "point"


instance ADTTag ShapeKind where
  type TagConstructor ShapeKind name = AppendSymbol "Kind" name


data ShapeKindSnake = Round | Boxy | Point
  deriving stock (Eq, Show, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindSnake


instance DBEnum ShapeKindSnake where
  enumTypeName = "shape_kind_snake"
  enumValue = \case
    Round -> "circle"
    Boxy -> "square"
    Point -> "dot"


type SnakeConstructor :: Symbol -> Symbol
type family SnakeConstructor name where
  SnakeConstructor "Circle" = "Round"
  SnakeConstructor "Square" = "Boxy"
  SnakeConstructor "Dot" = "Point"


instance ADTTag ShapeKindSnake where
  type TagConstructor ShapeKindSnake name = SnakeConstructor name


-- | The constructors in a different order from 'Shape''s: tags are paired by
-- 'TagConstructor', not by position.
data ShapeKindShuffled = ShuffledDot | ShuffledSquare | ShuffledCircle
  deriving stock (Eq, Show, Generic)
  deriving anyclass DBEq
  deriving DBType via Rel8.Enum ShapeKindShuffled


instance DBEnum ShapeKindShuffled where
  enumTypeName = "shape_kind_shuffled"
  enumValue = \case
    ShuffledDot -> "dot"
    ShuffledSquare -> "square"
    ShuffledCircle -> "circle"


instance ADTTag ShapeKindShuffled where
  type TagConstructor ShapeKindShuffled name = AppendSymbol "Shuffled" name


shapeKindTable :: TableSchema (ADTWith ShapeKind Shape Name)
shapeKindTable =
  TableSchema
    { name = "shape_kind_table"
    , columns = nameADTWith @ShapeKind @Shape "kind" "radius" "side" "colour"
    }


shapeKindSnakeTable :: TableSchema (ADTWith ShapeKindSnake Shape Name)
shapeKindSnakeTable =
  TableSchema
    { name = "shape_kind_snake_table"
    , columns = nameADTWith @ShapeKindSnake @Shape "kind" "radius" "side" "colour"
    }


shapeKindShuffledTable :: TableSchema (ADTWith ShapeKindShuffled Shape Name)
shapeKindShuffledTable =
  TableSchema
    { name = "shape_kind_shuffled_table"
    , columns = nameADTWith @ShapeKindShuffled @Shape "kind" "radius" "side" "colour"
    }


-- | An enum-tagged 'Shape' nested in an ordinary table, via 'HADTWith'.
data Labelled f = Labelled
  { label :: Column f Text
  , shape :: HADTWith ShapeKind f Shape
  }
  deriving stock Generic
  deriving anyclass Rel8able


deriving stock instance f ~ Result => Show (Labelled f)
deriving stock instance f ~ Result => Eq (Labelled f)
deriving stock instance f ~ Result => Ord (Labelled f)


genLabelled :: Hedgehog.MonadGen m => m (Labelled Result)
genLabelled = Labelled <$> Gen.text (Range.linear 0 10) Gen.alpha <*> genShape


labelledTable :: TableSchema (Labelled Name)
labelledTable =
  TableSchema
    { name = "labelled_table"
    , columns = Labelled
        { label = "label"
        , shape = nameADTWith @ShapeKind @Shape "kind" "radius" "side" "colour"
        }
    }


createEnumTypes :: [String]
createEnumTypes =
  [ "CREATE TYPE shape_kind AS ENUM ('round', 'boxy', 'point')"
  , "CREATE TYPE shape_kind_snake AS ENUM ('circle', 'square', 'dot')"
  , "CREATE TYPE shape_kind_shuffled AS ENUM ('dot', 'square', 'circle')"
  ]
