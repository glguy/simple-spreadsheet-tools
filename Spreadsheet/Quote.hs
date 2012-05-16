{-# LANGUAGE TemplateHaskell #-}
module Spreadsheet.Quote (table) where

import Data.Char (isAlphaNum)
import Data.Time (Day, fromGregorian, toGregorian)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parsec hiding (Column)
import Text.Parsec.Error

import Spreadsheet
import Spreadsheet.Parser (spreadsheetParser)

table :: QuasiQuoter
table = QuasiQuoter
  { quoteDec  = tableQ
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

-- 'tableQ' is a quasi-quoter for top-level spreadsheet declarations
-- using the ASCII art spreadsheet format.
tableQ :: String -> DecsQ
tableQ str =
  case parse declParser "[table|...|]" str of
    Right (name, ss) -> tableD name ss
    Left err -> do
      loc <- location
      fail (show (updateLocation loc err))

-- 'tableD' creates top-level spreadsheet declarations
tableD :: String -> Spreadsheet -> DecsQ
tableD name (Spreadsheet cols rows) =
  sequence
    {- data typeName = conName { col_..., ...} -}
    [ spreadsheetRecD typeName conName cols

    {- valName :: [typeName] -}
    , sigD valName (appT listT (conT typeName))

    {- valName = rows -}
    , valD (varP valName) (normalB (rowsE rows)) []
    ]
  where
  valName  = mkName     name
  typeName = toTypeName name
  conName  = toConName  name
  rowsE    = listE . map (spreadsheetRowE conName)

-- | 'updateLocation' combines the error location of a quasi-quoted
-- string and the error location inside the quasi-quoted string in
-- order to have an error position relative to the beginning of the
-- file.
updateLocation :: Loc -> ParseError -> ParseError
updateLocation loc err = setErrorPos newPos err
  where
  pos = errorPos err
  line' = sourceLine pos + fst (loc_start loc) - 1
  col'  = sourceColumn pos + snd (loc_start loc) - 1
  newPos | sourceLine pos == 1 = setSourceLine (setSourceColumn pos col') line'
         | otherwise = setSourceLine pos line'

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
