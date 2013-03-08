module Spreadsheet.Transform where

import Text.Parsec
import Data.List

import Spreadsheet
import Spreadsheet.Parser
import Spreadsheet.Renderer
import Spreadsheet.Sorting
import ListUtilities

transform :: (Spreadsheet -> Spreadsheet) -> IO ()
transform f = interact $ \ xs ->
  let cleaned = removeErrors xs in
  case parse spreadsheetParser "stdin" cleaned of
    Right res -> renderS (sortSpreadsheet (f res))
    Left  err -> unlines $ inlineError err $ lines cleaned

removeErrors :: String -> String
removeErrors = unlines . filter (not . isPrefixOf "!") . lines

inlineError :: ParseError -> [String] -> [String]
inlineError err xs = a ++ [errline] ++ b
  where
  (a,b) = splitAt (sourceLine (errorPos err)) xs

  spaceBeforeCaret = sourceColumn (errorPos err) - 2
  errline
    | length errText + 2 <= spaceBeforeCaret
                = '!' : padLeft (spaceBeforeCaret - 2) ' ' errText ++ "--^"
    | otherwise = '!' : replicate spaceBeforeCaret ' ' ++ "^--" ++ errText
  errText = replace '\n' ' ' . drop 1 . dropWhile (/= ':') . show $ err
