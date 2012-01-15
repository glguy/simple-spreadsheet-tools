module Parser where

import Text.Parsec
import Data.Char
import Control.Monad
import Control.Applicative ((<$),(<*))
import Numeric (readFloat)
import Data.Time (fromGregorianValid)

import Spreadsheet
import ListUtilities

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

extendFormats xs     (NewColumn:ys) = EmptyT : extendFormats xs ys
extendFormats (x:xs) (_:ys)         = x      : extendFormats xs ys
extendFormats _      ys             = map (const defaultType) ys

headerChar = satisfy (\x -> isPrint x && x `notElem` "[]<>+-\n") <?> "column name"

headerRow = many headerCell <* eol

headerCell = existingHeader <|> newHeader

existingHeader = between (startOfHeader >> white) (endOfHeader >> white)
  $ do name <- many headerChar
       order <- option Nothing (fmap Just sortOrder)
       return (HeaderToken (trim name) order)

sortOrder = (char '+' >> white >> return Ascending)
        <|> (char '-' >> white >> return Descending)

newHeader = char '+' >> white >> return NewColumn

formatCell = (string "text"   >> return StringT)
         <|> (string "number" >> fmap NumberT (optionMaybe (char ':' >> integer)))
         <|> (string "date"   >> return DateT)

dividerRow = skipMany1 (char '=') >> white >> eol

tableRow formats = between (return ()) eol (newrow formats <|> mapM dataCell formats)

newrow formats = char '+' >> return (map (const EmptyV) formats)

dataCell EmptyT = return EmptyV
dataCell fmt = between (startOfCell >> white) (endOfCell >> white)
             $ case fmt of
                 StringT   -> stringParser
                 NumberT _ -> numberParser
                 DateT     -> dateParser <?> "date"
            <|> return EmptyV

startOfHeader = char '<' <?> "start of header"
endOfHeader   = char '>' <?> "end of header"
startOfCell   = char '[' <?> "start of cell"
endOfCell     = char ']' <?> "end of cell"

stringParser = fmap (StringV . trim) (many1 dataChar)

numberParser = do
  negative <- option False (char '-' >> return True)
  x        <- many1 digit
  y        <- option "0" (char '.' >> many1 digit)
  let [(n,"")]         = readFloat (x ++ "." ++ y)
      n'   | negative  = negate n
           | otherwise = n
  white
  return (NumberV n')

dateParser = do
  year  <- integer
  del   <- oneOf "-/"
  month <- monthInteger
  _     <- char del
  day   <- dayInteger
  case fromGregorianValid year (fromIntegral month) (fromIntegral day) of
    Nothing   -> fail "Invalid date"
    Just date -> white >> return (DateV date)

monthInteger = do
  month <- integer
  when (month < 1 || month > 12) (fail "Invalid date")
  return month

dayInteger = do
  day <- integer
  when (day < 1 || day > 31) (fail "Invalid date")
  return day

integer = fmap read (many1 digit) <?> "number"

dataChar = satisfy (\x -> isPrint x && x `notElem` "[]\n") <?> "text"

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

white = skipMany (char ' ' <?> "whitespace")

eol   = newline >> spaces

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

headerTokenName (HeaderToken n _) = n
headerTokenName NewColumn         = ""

headerSortOrder (HeaderToken _ s) = s
headerSortOrder NewColumn         = Nothing
