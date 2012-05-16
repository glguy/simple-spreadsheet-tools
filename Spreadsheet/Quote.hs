{-# LANGUAGE TemplateHaskell #-}
module Spreadsheet.Quote (table) where

import Data.Char
import Data.Time
import Language.Haskell.TH.Quote
import Text.Parsec hiding (Column)
import Language.Haskell.TH

import Spreadsheet
import Spreadsheet.Parser

table :: QuasiQuoter
table = QuasiQuoter
  { quoteDec  = tableQ
  , quoteExp  = \_ -> fail "table: expressions not supported"
  , quotePat  = \_ -> fail "table: patterns not supported"
  , quoteType = \_ -> fail "table: types not supported"
  }


toConName, toFieldName :: String -> Name
toConName str   = mkName ("Row_" ++ str)
toFieldName str = mkName ("col_" ++ map escape str)
  where
  escape ' ' = '_'
  escape x   = x

declParser :: Parsec String () (String, Spreadsheet)
declParser = do
  spaces
  name <- many1 (satisfy isAlphaNum)
  spaces >> char '=' >> spaces
  ss <- spreadsheetParser
  return (name, ss)

tableQ :: String -> DecsQ
tableQ str = case parse declParser "quasiquoter" str of
  Left err -> fail ("table: Parse failed with error: " ++ show err)
  Right (name, Spreadsheet cols rows) -> do
    (recName, recDec) <- spreadsheetRecordQ name cols
    let rowsE = map (spreadsheetRowQ recName) rows
    valDec  <- valD (varP (mkName name)) (normalB (listE rowsE)) []
    return [recDec, valDec]

spreadsheetRowQ :: Name -> [CellValue] -> ExpQ
spreadsheetRowQ conName xs = appsE (conE conName : map toCell xs)

toCell :: CellValue -> ExpQ
toCell (NumberV x) = litE (rationalL x)
toCell (StringV x) = [| x |]
toCell (DateV   x) = dayE x
toCell EmptyV      = fail "Empty values not supported"

dayE :: Day -> ExpQ
dayE day = [|fromGregorian y m d|]
  where
  (y,m,d) = toGregorian day

spreadsheetRecordQ :: String -> [Column] -> Q (Name, Dec)
spreadsheetRecordQ nameStr xs = do
  let name = toConName nameStr
  let con = recC name (map toField xs)
  dec <- dataD (cxt []) name [] [con] [''Show,''Read,''Eq]
  return (name, dec)

toField :: Column -> VarStrictTypeQ
toField col = varStrictType
                   (toFieldName (columnName col))
                   (strictType notStrict (toFieldType (columnType col)))

toFieldType :: CellType -> TypeQ
toFieldType (NumberT _) = [t|Rational|]
toFieldType StringT     = [t|String|]
toFieldType DateT       = [t|Day|]
