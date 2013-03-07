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
  $ ic paddedHeader
  : ic headerSep
  : map ic paddedCells
  where
  ic = intercalate "|"
  -- Minimum width is 2 for the separator ":-"
  widths = map (max 2) $
      map maximum $ (map.map) length
    $ transpose $ colNames : renderCells

  paddedHeader = map (\(mx, x) -> padRight mx ' ' x) (zip widths colNames)

  headerSep = map (':':) lns
    where lns = map (flip replicate $ '-') (map (+(-1)) widths)

  renderCells = (map.map) (concatMap markDownShow . uncurry render)
                          (map (zip colTypes) cells)

  paddedCells :: [[String]]
  paddedCells = (map.map) dataPadmd expandedCells

  expandedCells :: [[(Int, String)]]
  expandedCells = map (zip widths) renderCells

  colNames = map (concatMap markDownShow . columnName) headers
  colTypes = map columnType headers

  dataPadmd :: (Int, String) -> String
  dataPadmd (mx, xs) = padRight mx ' ' xs

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
