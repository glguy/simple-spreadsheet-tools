module Main where

import Spreadsheet.Transform (transform)
import Spreadsheet.Sorting (sortSpreadsheet)

main :: IO ()
main = interact (transform sortSpreadsheet)
