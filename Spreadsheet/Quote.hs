{-# LANGUAGE TemplateHaskell #-}
module Spreadsheet.Quote (table) where

import Data.Char (isAlphaNum)
import Data.Time (Day, fromGregorian, toGregorian)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parsec hiding (Column)

import Spreadsheet
import Spreadsheet.Parser (spreadsheetParser)

table :: QuasiQuoter
table = QuasiQuoter
  { quoteDec  = tableD
  , quoteExp  = \_ -> fail "table: expressions not supported"
  , quotePat  = \_ -> fail "table: patterns not supported"
  , quoteType = \_ -> fail "table: types not supported"
  }

toTypeName, toConName, toFieldName :: String -> Name
toTypeName  str = mkName ("Row_" ++ str)
toConName   str = mkName ("Row_" ++ str)
toFieldName str = mkName ("col_" ++ map escape str)
  where
  escape ' ' = '_'
  escape x   = x

declParser :: Parsec String () (String, Spreadsheet)
declParser = do
  name <- spaces >> identifier
  _    <- spaces >> char '='
  ss   <-           spreadsheetParser
  return (name, ss)
  where
  identifier = many1 (satisfy isAlphaNum) <?> "identifier"

tableD :: String -> DecsQ
tableD str =
  case parse declParser "quasiquoter" str of
    Left err -> fail ("table: Parse failed with error:\n" ++ show err)
    Right (name, Spreadsheet cols rows) ->
      sequence
        [ spreadsheetRecD typeName conName cols

        , sigD valName        {- :: -} (appT listT (conT typeName))
        , valD (varP valName) {- =  -} (normalB (rowsE rows)) []
        ]
      where
      valName  = mkName     name
      typeName = toTypeName name
      conName  = toConName  name
      rowsE    = listE . map (spreadsheetRowE conName)

spreadsheetRowE :: Name -> [CellValue] -> ExpQ
spreadsheetRowE conName xs = appsE (conE conName : map cellValueE xs)

cellValueE :: CellValue -> ExpQ
cellValueE (NumberV x) = litE (rationalL x)
cellValueE (StringV x) = [| x |]
cellValueE (DateV   x) = dayE x
cellValueE EmptyV      = fail "Empty values not supported"

dayE :: Day -> ExpQ
dayE day = [|fromGregorian y m d|]
  where
  (y,m,d) = toGregorian day

spreadsheetRecD :: Name -> Name -> [Column] -> DecQ
spreadsheetRecD typeName conName xs =
  dataD (cxt []) typeName [] [recC conName fields] [''Show,''Read,''Eq]
  where
  fields = map toField xs

  toField col =
    varStrictType
      (toFieldName (columnName col))
      (strictType notStrict (cellTypeT (columnType col)))

cellTypeT :: CellType -> TypeQ
cellTypeT (NumberT _) = [t|Rational|]
cellTypeT StringT     = [t|String|]
cellTypeT DateT       = [t|Day|]
