module Scheme where

import           Text.ParserCombinators.Parsec hiding (spaces)
-- import System.Environment
-- import Control.Monad
import           Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving Show

parseStringChar :: Parser Char
parseStringChar = (char '\\' >> char '"') <|> (noneOf "\"")

parseString :: Parser LispVal
parseString =
   do
      _ <- char '"'
      x <- many parseStringChar
      _ <- char '"'
      return $ String x

parseAtom :: Parser LispVal
parseAtom =
   do
      first <- letter <|> symbol
      rest <- many (letter <|> digit <|> symbol)
      let atom = first:rest
      return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- ex1.1 parseNumber using do
-- parseNumber :: Parser LispVal
-- parseNumber =
--    do
--       digits <- many1 digit
--       return $ (Number . read) digits

-- ex1.2 with >>=
parseNumber :: Parser LispVal
--parseNumber = many1 digit >>= (\a -> return (Number . read) a)
parseNumber = parseOctal <|> parseHex <|> (many1 digit >>= return . Number . read)

-- parse an octal number
parseOctal :: Parser LispVal
parseOctal =
   do
      _ <- char '#'
      _ <- char 'o'
      nums <- fmap (fst . head . readOct) (many1 octDigit)
      return $ Number nums

-- parse a hex number
parseHex :: Parser LispVal
parseHex =
   do
      _ <- char '#'
      _ <- char 'x'
      nums <- fmap (fst . head . readHex) (many1 hexDigit)
      return $ Number nums

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value" ++ show val

spaces :: Parser ()
spaces = skipMany1 space

