module Transform where

import Text.Parsec
import Data.List

import Parser
import Renderer
import Spreadsheet
import Sorting
import ListUtilities

transform f = interact $ \ xs ->
  let cleaned = removeErrors xs in
  case runParser spreadsheetParser () "stdin" cleaned of
    Right res -> renderS (sortSpreadsheet (f res))
    Left  err -> unlines $ inlineError err $ lines cleaned

removeErrors = unlines . filter (not . isPrefixOf "!") . lines

inlineError err xs = a ++ [errline] ++ b
  where
  (a,b) = splitAt (sourceLine (errorPos err)) xs

  spaceBeforeCaret = sourceColumn (errorPos err) - 2
  errline
    | length errText + 2 <= spaceBeforeCaret
                = '!' : padLeft (spaceBeforeCaret - 2) ' ' errText ++ "--^"
    | otherwise = '!' : replicate spaceBeforeCaret ' ' ++ "^--" ++ errText
  errText = replace '\n' ' ' . drop 1 . dropWhile (/= ':') . show $ err
