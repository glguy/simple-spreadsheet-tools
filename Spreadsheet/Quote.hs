{-# LANGUAGE TemplateHaskell #-}
module Spreadsheet.Quote (table) where

import Data.Char
import Data.Time
import Language.Haskell.TH.Quote
import Text.Parsec hiding (Column)
import qualified Language.Haskell.TH as TH

import Parser
import Spreadsheet

table :: QuasiQuoter
table = QuasiQuoter
  { quoteDec = tableQ
  , quoteExp = \_ -> fail "table: expressions not supported"
  , quotePat = \_ -> fail "table: patterns not supported"
  , quoteType = \_ -> fail "table: types not supported"
  }


toConName, toFieldName :: String -> TH.Name
toConName str   = TH.mkName ("Row_" ++ str)
toFieldName str = TH.mkName ("col_" ++ map escape str)
  where
  escape ' ' = '_'
  escape x   = x

declParser = do
  spaces
  name <- many1 (satisfy isAlphaNum)
  spaces >> char '=' >> spaces
  ss <- spreadsheetParser
  return (name, ss)

tableQ :: String -> TH.Q [TH.Dec]
tableQ str = case parse declParser "quasiquoter" str of
  Left err -> fail ("table: Parse failed with error: " ++ show err)
  Right (name, Spreadsheet cols rows) -> do
    (recName, recDec) <- spreadsheetRecordQ name cols
    let rowsE = map (spreadsheetRowQ recName) rows
    valDec  <- TH.valD (TH.varP (TH.mkName name)) (TH.normalB (TH.listE rowsE)) []
    return [recDec, valDec]

spreadsheetRowQ conName xs = TH.appsE (TH.conE conName : map toCell xs)

toCell (NumberV x) = TH.litE (TH.rationalL x)
toCell (StringV x) = [| x |]
toCell (DateV   x) = dayE x
toCell EmptyV      = fail "Empty values not supported"

dayE :: Day -> TH.ExpQ
dayE day = [|fromGregorian y m d|]
  where
  (y,m,d) = toGregorian day

spreadsheetRecordQ :: String -> [Column] -> TH.Q (TH.Name, TH.Dec)
spreadsheetRecordQ nameStr xs = do
  let name = toConName nameStr
  let con = TH.recC name (map toField xs)
  dec <- TH.dataD (TH.cxt []) name [] [con] [''Show,''Read,''Eq]
  return (name, dec)

toField col = TH.varStrictType
                   (toFieldName (columnName col))
                   (TH.strictType TH.notStrict (toFieldType (columnType col)))

toFieldType (NumberT _) = [t|Rational|]
toFieldType StringT     = [t|String|]
toFieldType DateT       = [t|Day|]
toFieldType EmptyT      = error "toFieldType: EmptyT not supported"
