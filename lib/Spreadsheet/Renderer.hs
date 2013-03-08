module Spreadsheet.Renderer where

import Data.List
import Data.Ratio
import Numeric

import Spreadsheet
import ListUtilities

render :: CellType -> CellValue -> String
render StringT           (StringV xs) = xs
render (NumberT Nothing) (NumberV n)  | denominator n == 1 = show (numerator n)
render (NumberT p)       (NumberV n)  = showFFloat (fmap fromInteger p) (fromRational n :: Double) ""
render DateT             (DateV d)    = show d
render _                 EmptyV       = ""
render _                 _            = "!!!"

renderT :: CellType -> String
renderT StringT     = "text"
renderT (NumberT p) = "number" ++ maybe "" ((':':).show) p
renderT DateT       = "date"

renderS :: Spreadsheet -> String
renderS (Spreadsheet hfs rs)
  = unlines
  $ intercalate " " (zipWith3 headerPad widths hs ss)
  : intercalate " " (zipWith formatPad widths fTexts)
  : replicate (sum widths + length widths - 1) '='
  : map (intercalate " " . zipWith3 dataPad widths fs) rTexts
  where
  widths = map maximum $ transpose $ zipWith (\h s -> length h + 4 + sortLength s) hs ss
                                   : map length fTexts
                                   : map (map (\d -> length d + 4)) rTexts

  hs = map columnName hfs
  fs = map columnType hfs
  ss = map columnSort hfs

  fTexts = map renderT fs

  rTexts = map (zipWith render fs) rs

  sortLength Nothing  = 0
  sortLength (Just _) = 2

-- | Render the spreadsheet in markdown.
renderSmd :: Spreadsheet -> String
renderSmd (Spreadsheet headers cells)
  = unlines
  $ map (intercalate "|")
  $ padRow colNames
  : headerSep
  : map padRow renderedCells
  where
  -- Minimum width is 2 for the separator ":-"
  widths = (map (max 2 . maximum . map length) . transpose)
           (colNames : renderedCells)

  mkSep w = ':' : replicate (w-1) '-'
  headerSep = map mkSep widths

  colTypes = map columnType headers
  colNames = map (escapeString . columnName) headers

  renderedCells = map renderRow cells

  padRow       = zipWith (\w -> padRight w ' ') widths
  renderRow    = map escapeString . zipWith render colTypes
  escapeString = concatMap markDownShow

markDownShow :: Char -> String
markDownShow c =
  case c of
    '='  -> esc
    '*'  -> esc
    '{'  -> esc
    '}'  -> esc
    '['  -> esc
    ']'  -> esc
    '('  -> esc
    ')'  -> esc
    '#'  -> esc
    '+'  -> esc
    '_'  -> esc
    '.'  -> esc
    '!'  -> esc
    _    -> [c]
  where esc = ['\\',c]

headerPad :: Int -> String -> Maybe SortOrder -> String
headerPad i xs Nothing  = "< " ++ padRight (i - 4) ' ' xs ++ " >"
headerPad i xs (Just s) = "< " ++ padRight (i - 6) ' ' xs ++ " " ++ sortStr ++ " >"
  where
  sortStr = case s of
              Ascending  -> "+"
              Descending -> "-"

formatPad :: Int -> String -> String
formatPad i xs = padRight i ' ' xs


dataPad :: Int -> CellType -> String -> String
dataPad i StringT     xs = "[ " ++ padRight (i - 4) ' ' xs ++ " ]"
dataPad i DateT       xs = "[ " ++ padRight (i - 4) ' ' xs ++ " ]"
dataPad i (NumberT _) xs = "[ " ++ padLeft  (i - 4) ' ' xs ++ " ]"
