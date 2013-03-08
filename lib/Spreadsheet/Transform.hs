module Spreadsheet.Transform where

import Text.Parsec
import Data.List

import Spreadsheet
import Spreadsheet.Parser
import Spreadsheet.Renderer
import ListUtilities

-- | Parse a spreadsheet, apply a transformation, and rerender the
-- result. Parse errors will be inlined into the untransformed
-- output. Errors are ignored when performing the transformation.
transform :: (Spreadsheet -> Spreadsheet) -> String -> String
transform f xs =
  let cleaned = removeErrors xs in
  case parse spreadsheetParser "stdin" cleaned of
    Right res -> renderSpreadsheet (f res)
    Left  err -> inlineError err cleaned

-- | Remove inlined errors from a spreadsheet
removeErrors :: String -> String
removeErrors = unlines . filter (not . isPrefixOf "!") . lines

-- | Insert an error into spreadsheet pointing at the position
-- of the error.
inlineError :: ParseError -> String -> String
inlineError err xs = unlines (a ++ [errline] ++ b)
  where
  (a,b) = splitAt (sourceLine (errorPos err)) (lines xs)

  spaceBeforeCaret = sourceColumn (errorPos err) - 2
  errline
    | length errText + 2 <= spaceBeforeCaret
                = '!' : padLeft (spaceBeforeCaret - 2) ' ' errText ++ "--^"
    | otherwise = '!' : replicate spaceBeforeCaret ' ' ++ "^--" ++ errText
  errText = replace '\n' ' ' . drop 1 . dropWhile (/= ':') . show $ err
