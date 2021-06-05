module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
-- import qualified Data.Map as M
import Control.Monad
import Data.Functor

data LispVal = Atom String
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving Show

parseStringChar :: Parser Char
parseStringChar = (char '\\' >> char '"') <|> noneOf "\""

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

-- given a map of character names and the char they map to
-- parse them (todo)

parseNamedCharacter :: Parser LispVal
parseNamedCharacter =
    do 
        try $ string "#\\space"
        return $ Character ' '

parseCharacterSingle :: Parser LispVal
parseCharacterSingle = 
    do
        try (string "#\\")
        c <- try (letter <|> digit)
        return $ Character c

parseCharacter :: Parser LispVal
parseCharacter = parseNamedCharacter <|> parseCharacterSingle

parseNumber :: Parser LispVal
parseNumber =
    parseHex <|> 
    parseOctal <|>
    (many1 digit <&> Number . read)

-- parse an octal number
parseOctal :: Parser LispVal
parseOctal =
   do
      try (string "#o")
      nums <- fmap (fst . head . readOct) (many1 octDigit)
      return $ Number nums

-- parse a hex number
parseHex :: Parser LispVal
parseHex =
   do
      try (string "#x")
      nums <- fmap (fst . head . readHex) (many1 hexDigit)
      return $ Number nums

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseAtom
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Parse the backtick (quasiquote)


main :: IO ()
main =
    do
        (expr: _) <- getArgs
        putStrLn (readExpr expr)

