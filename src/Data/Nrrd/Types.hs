module Data.Nrrd.Types
  (
    Key
  , Value (..)
  , SpaceDirections (..)
  , Tuple3
  , Space (..)
  , KVPs
  ) where

import qualified Data.Map as M


-- Basic field specifications
--
-- data DataType
--   = UInt1 | Int1 | UInt2 | Int2 | UInt4 | Int4 | UInt8 | Int8 | Float4 | Float8
--   deriving (Show, Eq)
-- type DataType = String
-- type Encoding = String


-- Space and orientation
--
data Space = LPS | RPS | RAS | LAS
  deriving (Show, Eq)

type Tuple3 = (Double, Double, Double)

data SpaceDirections
  = StructuralSpace Tuple3 Tuple3 Tuple3
  | DWISpace Tuple3 Tuple3 Tuple3
  deriving (Eq, Show)

-- type SpaceOrigin = Tuple3
-- type Dimension = Int


-- Per axis specifications
--
-- type Sizes = [Integer]
-- type Kinds = [String]


type Key = String

data Value
  = VDataType String
  | VDimension Integer
  | VSpace Space
  | VSizes [Integer]
  | VSpaceDirections SpaceDirections
  | VKinds [String]
  | VEndian String
  | VEncoding String
  | VSpaceOrigin Tuple3
  | VGradientDir Double Double Double
  | VMeasurementFrame Tuple3 Tuple3 Tuple3
  | VDefault String
  deriving (Show, Eq)

type KVPs = M.Map Key Value
