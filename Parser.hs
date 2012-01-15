module Parser where

import Control.Applicative ((<$),(<*))
import Control.Monad       (when)
import Data.Char           (isPrint, isSpace)
import Data.Time           (Day, fromGregorianValid)
import Numeric             (readFloat)
import Text.Parsec

import Spreadsheet
import ListUtilities

defaultType :: CellType
defaultType = StringT

spreadsheetParser :: Parsec String () Spreadsheet
spreadsheetParser = do
  spaces
  headings <- headerRow
  formats  <- option [] (wordsRow formatCell)
  optional dividerRow

  let headings' = map headerTokenName headings 
  let formats'  = extendFormats formats headings
  let formats'' = replace EmptyT defaultType formats'
  let orders    = map headerSortOrder headings

  rows <- many (tableRow formats')
  spaces
  eof

  let columns = zipWith3 Column headings' formats'' orders
  return (Spreadsheet columns rows)

-- | 'extendFormats' deals with creating new format types
-- for newly created columns and for defaulting unspecified
-- columns to the 'defaultType'
extendFormats :: [CellType] -> [HeaderToken] -> [CellType]
extendFormats xs     (NewColumn:ys) = EmptyT : extendFormats xs ys
extendFormats (x:xs) (_:ys)         = x      : extendFormats xs ys
extendFormats _      ys             = map (const defaultType) ys

-- | 'headerChar' parses characters with are valid in header names.
headerChar :: Parsec String () Char
headerChar = satisfy (\x -> isPrint x && x `notElem` "<>+-\n") <?> "column name"

headerRow :: Parsec String () [HeaderToken]
headerRow = many headerCell <* eol

headerCell :: Parsec String () HeaderToken
headerCell = existingHeader <|> newHeader

existingHeader :: Parsec String () HeaderToken
existingHeader = between (startOfHeader >> white) (endOfHeader >> white)
  $ do name <- many headerChar
       order <- option Nothing (fmap Just sortOrder)
       return (HeaderToken (trim name) order)

sortOrder :: Parsec String () SortOrder
sortOrder = (char '+' >> white >> return Ascending)
        <|> (char '-' >> white >> return Descending)

newHeader :: Parsec String () HeaderToken
newHeader = char '+' >> white >> return NewColumn

formatCell :: Parsec String () CellType
formatCell = (string "text"   >> return StringT)
         <|> (string "number" >> fmap NumberT (optionMaybe (char ':' >> integer)))
         <|> (string "date"   >> return DateT)

dividerRow :: Parsec String () ()
dividerRow = skipMany1 (char '=') >> white >> eol

tableRow :: [CellType] -> Parsec String () [CellValue]
tableRow formats = (newRow formats <|> mapM dataCell formats) <* eol

newRow :: [CellType] -> Parsec String () [CellValue]
newRow formats = char '+' >> return (map (const EmptyV) formats)

dataCell :: CellType -> Parsec String () CellValue
dataCell EmptyT = return EmptyV
dataCell fmt = between (startOfCell >> white) (endOfCell >> white)
             $ case fmt of
                 StringT   -> fmap StringV stringParser
                 NumberT _ -> fmap NumberV numberParser
                 DateT     -> fmap DateV   dateParser
            <|> return EmptyV

startOfHeader, endOfHeader, startOfCell, endOfCell
  :: Parsec String () Char
startOfHeader = char '<' <?> "start of header"
endOfHeader   = char '>' <?> "end of header"
startOfCell   = char '[' <?> "start of cell"
endOfCell     = char ']' <?> "end of cell"

stringParser :: Parsec String () String
stringParser = fmap trim (many1 dataChar)

numberParser :: Parsec String () Rational
numberParser = do
  negative <- option False (char '-' >> return True)
  x        <- many1 digit
  y        <- option "0" (char '.' >> many1 digit)
  let [(n,"")]         = readFloat (x ++ "." ++ y)
      n'   | negative  = negate n
           | otherwise = n
  white
  return n'

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

monthInteger :: Parsec String () Integer
monthInteger = do
  month <- integer
  when (month < 1 || month > 12) (fail "Invalid date")
  return month

dayInteger :: Parsec String () Integer
dayInteger = do
  day <- integer
  when (day < 1 || day > 31) (fail "Invalid date")
  return day

integer :: Parsec String () Integer
integer = fmap read (many1 digit) <?> "number"

dataChar :: Parsec String () Char
dataChar = satisfy (\x -> isPrint x && x `notElem` "[]\n") <?> "text"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

white :: Parsec String () ()
white = skipMany (char ' ' <?> "whitespace")

eol :: Parsec String () ()
eol = newline >> spaces

wordsRow :: Parsec String () a -> Parsec String () [a]
wordsRow p = onemore <|> ([] <$ eol)
  where
  onemore = do
    x <- p
    xs <- (skipMany1 (char ' ') >> wordsRow p)
      <|> ([] <$ eol)
    return (x:xs)

data HeaderToken
  = NewColumn
  | HeaderToken String (Maybe SortOrder)
  deriving (Read, Show, Eq)

headerTokenName :: HeaderToken -> String
headerTokenName (HeaderToken n _) = n
headerTokenName NewColumn         = ""

headerSortOrder :: HeaderToken -> Maybe SortOrder
headerSortOrder (HeaderToken _ s) = s
headerSortOrder NewColumn         = Nothing
