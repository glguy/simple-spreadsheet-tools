module Sorting where

import Spreadsheet
import Data.List
import Data.Monoid

sortSpreadsheet (Spreadsheet hs xs) = Spreadsheet hs (sortBy rowCompare xs)
  where
  ss = map columnSort hs

  rowCompare a b = mconcat (zipWith3 aux ss a b)

  aux Nothing           _ _ = EQ
  aux (Just Ascending)  a b = compareData a b
  aux (Just Descending) a b = compareData b a


compareData EmptyV EmptyV = EQ
compareData EmptyV _      = LT
compareData _      EmptyV = GT

compareData (StringV xs) (StringV ys) = compare xs ys
compareData (NumberV xs) (NumberV ys) = compare xs ys
compareData (DateV   xs) (DateV   ys) = compare xs ys

compareData _ _ = EQ
