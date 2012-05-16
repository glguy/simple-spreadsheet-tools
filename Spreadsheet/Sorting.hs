module Spreadsheet.Sorting where

import Data.List
import Data.Monoid

import Spreadsheet

-- | 'sortSpreadsheet' sorts the rows of a spreadsheet
-- given the sort orders specified in the header row
sortSpreadsheet :: Spreadsheet -> Spreadsheet
sortSpreadsheet (Spreadsheet hs xs) = Spreadsheet hs (sortBy rowCompare xs)
  where
  ss = map columnSort hs

  rowCompare a b = mconcat (zipWith3 aux ss a b)

  aux Nothing           _ _ = EQ
  aux (Just Ascending)  a b = compareData a b
  aux (Just Descending) a b = compareData b a


-- | 'compareData' compares two data cells using the underlying
-- given the sort orders specified in the header row.
-- Empty cells are considered the smallest and incomparable cells
-- are considered equal (as this should not happen in real use)
compareData :: CellValue -> CellValue -> Ordering
compareData EmptyV       EmptyV       = EQ
compareData EmptyV       _            = LT
compareData _            EmptyV       = GT
compareData (StringV xs) (StringV ys) = compare xs ys
compareData (NumberV xs) (NumberV ys) = compare xs ys
compareData (DateV   xs) (DateV   ys) = compare xs ys
compareData _            _            = EQ
