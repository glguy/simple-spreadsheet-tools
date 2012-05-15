module Spreadsheet where

import Data.Time (Day)

data CellType
  = NumberT (Maybe Integer)
  | StringT
  | DateT
  | EmptyT
  deriving (Show, Read, Eq)

data Column = Column
  { columnName :: String
  , columnType :: CellType
  , columnSort :: Maybe SortOrder
  }
  deriving (Eq, Read, Show)

data CellValue
  = NumberV Rational
  | StringV String
  | DateV Day
  | EmptyV
  deriving (Show, Read, Eq)

data SortOrder
  = Ascending
  | Descending
  deriving (Show, Read, Eq)

data Spreadsheet = Spreadsheet [Column] [[CellValue]]
  deriving (Show, Read, Eq)


