module Spreadsheet.Renderer where

import Data.List
import Data.Ratio
import Numeric

import Spreadsheet
import ListUtilities

renderCell :: CellType -> CellValue -> String
renderCell StringT           (StringV xs) = xs
renderCell (NumberT Nothing) (NumberV n)  | denominator n == 1 = show (numerator n)
renderCell (NumberT p)       (NumberV n)  = showFFloat (fmap fromInteger p) (fromRational n :: Double) ""
renderCell DateT             (DateV d)    = show d
renderCell _                 EmptyV       = ""
renderCell _                 _            = "!!!"

renderCellType :: CellType -> String
renderCellType StringT     = "text"
renderCellType (NumberT p) = "number" ++ maybe "" ((':':).show) p
renderCellType DateT       = "date"

renderSpreadsheet :: Spreadsheet -> String
renderSpreadsheet (Spreadsheet hfs rs)
  = unlines
  $ map (intercalate " ")
  $ zipWith3 headerPad widths hs ss
  : zipWith formatPad widths fTexts
  : [headerBar]
  : map (zipWith3 dataPad widths fs) rTexts
  where
  widths = map maximum
         $ transpose
         $ map headerWidth hfs
         : map length fTexts
         : map (map cellWidth) rTexts

  headerBar = replicate (sum widths + length widths - 1) '='

  hs = map columnName hfs
  fs = map columnType hfs
  ss = map columnSort hfs

  headerWidth h = length (columnName h) + sortLength (columnSort h) + 4
  -- extra 4 because header is wrapped by '< ' and ' >'

  cellWidth d = length d + 4
  -- extra 4 because cell is wrapped by '[ ' and ' ]'

  fTexts = map renderCellType fs

  rTexts = map (zipWith renderCell fs) rs

  sortLength Nothing  = 0 -- ''
  sortLength (Just _) = 2 -- ' +' and ' -'

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
  renderRow    = map escapeString . zipWith renderCell colTypes
  escapeString = concatMap markDownEscape

markDownEscape :: Char -> String
markDownEscape c =
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
    '|'  -> esc
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
dataPad i cellType xs = "[ " ++ pad (i - 4) ' ' xs ++ " ]"
  where
  pad = case cellType of
    StringT   -> padRight
    DateT     -> padRight
    NumberT _ -> padLeft
