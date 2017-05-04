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
  = StructuralSpaceDirections Tuple3 Tuple3 Tuple3
  | DWISpaceDirections Tuple3 Tuple3 Tuple3
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
  | VGradientDir Tuple3
  | VMeasurementFrame Tuple3 Tuple3 Tuple3
  | VDefault String
  deriving (Show, Eq)

-- instance Show Value where
--   show (VDataType s) = s
--   show (VEndian s) = s
--   show (VEncoding s) = s
--   show (VMeasurementFrame x1 x2 x3) = unwords [show x1, show x2, show x3]
--   show (VDefault s) = s
--   show x = unwords . tail . words. show $ x


type KVPs = M.Map Key Value
