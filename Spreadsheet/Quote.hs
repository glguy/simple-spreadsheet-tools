{-# LANGUAGE TemplateHaskell #-}
module Spreadsheet.Quote (table) where

import Data.Char (isAlphaNum)
import Data.Time (Day, fromGregorian, toGregorian)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Parsec hiding (Column)
import Text.Parsec.Error
import Text.Parsec.Pos (newPos)

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
tableQ str = do
  loc <- location
  case parse (setLoc loc >> declParser) undefined str of
    Right (name, ss) -> tableD name ss
    Left err         -> fail (show err)

-- | 'setLoc' resets the local parser location to the encompassing
-- file location.
setLoc :: Loc -> Parsec String () ()
setLoc loc = setPosition pos
  where
  pos         = newPos (loc_filename loc) line col
  (line, col) = loc_start loc

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
