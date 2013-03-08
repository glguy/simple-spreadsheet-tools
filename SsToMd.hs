module Main where

import Spreadsheet.Parser (spreadsheetParser)
import Spreadsheet.Renderer (renderSmd)

import Text.Parsec (parse)

main :: IO ()
main = interact $ \ xs ->
  case parse spreadsheetParser "stdin" xs of
    Right res -> renderSmd res
    Left  err -> fail (show err)
