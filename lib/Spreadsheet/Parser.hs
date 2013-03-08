module Spreadsheet.Parser where

import Control.Applicative ((<$),(<*))
import Control.Monad       (when)
import Data.Char           (isPrint, isSpace)
import Data.List           (dropWhileEnd)
import Data.Maybe          (fromMaybe)
import Data.Time           (Day, fromGregorianValid)
import Numeric             (readFloat)
import Text.Parsec

import Spreadsheet

-- | 'defaultType' is the type that columns will default to when
-- not otherwise specified
defaultType :: CellType
defaultType = StringT

-- | Parse a full spread-sheet file. This parser expects to use the
-- whole input.
spreadsheetParser :: Parsec String () Spreadsheet
spreadsheetParser = do
  spaces
  headings <- headerRow
  formats  <- option [] (sepEndBy1 formatCell white1 <* eol)
  optional dividerRow

  let headings' = map headerTokenName headings
  let formats'  = extendFormats (formats ++ repeat defaultType) headings
  let formats'' = map (fromMaybe defaultType) formats'
  let orders    = map headerSortOrder headings

  rows <- many (tableRow formats')
  spaces
  eof

  let columns = zipWith3 Column headings' formats'' orders
  return (Spreadsheet columns rows)

namedSpreadsheetParser :: Parsec String () (String, Spreadsheet)
namedSpreadsheetParser = do
  name <- spaces >> identifier
  _    <- spaces >> char '='
  ss   <-           spreadsheetParser
  return (name, ss)
  where
  identifier = many1 alphaNum <?> "identifier"


-- | 'extendFormats' deals with creating new format types
-- for newly created columns and for defaulting unspecified
-- columns to the 'defaultType'
extendFormats :: [CellType] -> [HeaderToken] -> [Maybe CellType]
extendFormats xs     (NewColumn:ys) = Nothing : extendFormats xs ys
extendFormats (x:xs) (_:ys)         = Just x  : extendFormats xs ys
extendFormats _      []             = []

-- | 'headerChar' parses characters with are valid in header names.
headerChar :: Parsec String () Char
headerChar = satisfy (\x -> isPrint x && x `notElem` "<>+-\n") <?> "column name"

-- | 'headerRow' the header line of the spreadsheet
headerRow :: Parsec String () [HeaderToken]
headerRow = many headerCell <* eol

-- | 'headerCell' parses a single header entry in the header line
-- or the "new header" placeholder
headerCell :: Parsec String () HeaderToken
headerCell = existingHeader <|> newHeader

-- | 'headerCell' parses a single header entry in the header line
existingHeader :: Parsec String () HeaderToken
existingHeader = between startOfHeader endOfHeader
   (do name <- many1 headerChar
       order <- option Nothing (fmap Just sortOrder)
       return (HeaderToken (trim name) order)
   ) <?> "column header (<...>)"

-- | 'sortOrder' parses the ascending/descending indicator
sortOrder :: Parsec String () SortOrder
sortOrder = (Ascending  <$ sortAscTok )
        <|> (Descending <$ sortDescTok)

-- | 'newHeader' parses the new header placeholder
newHeader :: Parsec String () HeaderToken
newHeader = NewColumn <$ newColTok

-- | 'formatCell' parses the "type" field of a column
formatCell :: Parsec String () CellType
formatCell = (string "text"   >> return StringT)
         <|> (string "number" >> fmap NumberT (optionMaybe (char ':' >> integer)))
         <|> (string "date"   >> return DateT)

-- | 'dividerRow' parses the line drawn between headers and body
dividerRow :: Parsec String () ()
dividerRow = skipMany1 (char '=') >> white >> eol
         <?> "divider line (===)"

-- | 'tableRow' parses a whole row of data cells
tableRow :: [Maybe CellType] -> Parsec String () [CellValue]
tableRow formats = (newRow formats <|> mapM dataCell formats) <* eol

-- | 'newRow' parses a new row placeholder
newRow :: [Maybe CellType] -> Parsec String () [CellValue]
newRow formats = map (const EmptyV) formats <$ newRowTok

-- | 'dataCell' parses an individual data cell in a data row
dataCell :: Maybe CellType -> Parsec String () CellValue
dataCell Nothing = return EmptyV
dataCell (Just fmt) = between startOfCell endOfCell
             ( case fmt of
                 StringT   -> fmap StringV stringParser
                 NumberT _ -> fmap NumberV numberParser
                 DateT     -> fmap DateV   dateParser
            <|> return EmptyV
             ) <?> "value cell ([...])"

startOfHeader, endOfHeader, startOfCell, endOfCell
  :: Parsec String () ()
startOfHeader = tok '<' <?> "start of column header (<)"
endOfHeader   = tok '>' <?> "end of column header (>)"
startOfCell   = tok '[' <?> "start of value cell ([)"
endOfCell     = tok ']' <?> "end of value cell (])"
newRowTok     = tok '+' <?> "new row marker (+)"
newColTok     = tok '+' <?> "new column marker (+)"
sortAscTok    = tok '+' <?> "sort ascending (+)"
sortDescTok   = tok '-' <?> "sort descending (-)"

-- | Consume a token character and any trailing white space.
tok :: Char -> Parsec String () ()
tok c = char c >> white

-- | 'stringParser' parses a string of allowed data characters
stringParser :: Parsec String () String
stringParser = fmap trim (many1 dataChar)

-- | 'numberParser' parses data cells holding number fields
numberParser :: Parsec String () Rational
numberParser = do
  negative <- option False (True <$ char '-')
  x        <- many1 digit
  y        <- option "0" (char '.' >> many1 digit)
  let [(n,"")]         = readFloat (x ++ "." ++ y)
      n'   | negative  = negate n
           | otherwise = n
  white
  return n'

-- | 'dateParser' parses data cells holding date fields
dateParser :: Parsec String () Day
dateParser = do
  year  <- integer
  del   <- oneOf "-/"
  month <- monthInteger
  _     <- char del
  day   <- dayInteger
  case fromGregorianValid year (fromIntegral month) (fromIntegral day) of
    Nothing   -> fail "Invalid date"
    Just date -> white >> return date

-- | 'dayInteger' parses number between 1 and 12
monthInteger :: Parsec String () Integer
monthInteger = do
  month <- integer
  when (month < 1 || month > 12) (fail "Invalid date")
  return month

-- | 'dayInteger' parses number between 1 and 31
dayInteger :: Parsec String () Integer
dayInteger = do
  day <- integer
  when (day < 1 || day > 31) (fail "Invalid date")
  return day

-- | 'integer' parses a whole number
integer :: Parsec String () Integer
integer = fmap read (many1 digit) <?> "integer"

-- | 'dataChar' parses a legal character in string fields
dataChar :: Parsec String () Char
dataChar = satisfy (\x -> isPrint x && x `notElem` "[]\n") <?> "text"

-- | 'trim' removes leading and trailing whitespace on a string
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- | 'white' skips spaces and tabs
white :: Parsec String () ()
white = skipMany (oneOf " \t" <?> "white space")

-- | 'white1' skips at least one of spaces and tabs
white1 :: Parsec String () ()
white1 = skipMany1 (oneOf " \t" <?> "white space")

-- | 'eol' parses the end-of-line token
eol :: Parsec String () ()
eol = newline >> spaces

data HeaderToken
  = NewColumn
  | HeaderToken String (Maybe SortOrder)
  deriving (Read, Show, Eq)

-- | 'headerTokenName' returns the name of a 'HeaderToken'
headerTokenName :: HeaderToken -> String
headerTokenName (HeaderToken n _) = n
headerTokenName NewColumn         = ""

-- | 'headerTokenName' returns the sort order of a 'HeaderToken'
headerSortOrder :: HeaderToken -> Maybe SortOrder
headerSortOrder (HeaderToken _ s) = s
headerSortOrder NewColumn         = Nothing
