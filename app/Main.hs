module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

data LispVal = Atom String
             | Character Char
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
parseNumber =
    parseHex <|> 
    parseOctal <|>
    (many1 digit >>= return . Number . read)

parseCharacter :: Parser LispVal
parseCharacter = 
    do
        try (string "#\\")
        c <- try (letter <|> digit)
        return $ Character c

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
parseExpr = 
        parseCharacter <|>
        parseNumber <|> 
        parseString <|>
        parseAtom

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main =
    do
        (expr: _) <- getArgs
        putStrLn (readExpr expr)

-- prompt for two numbers and add them 
main3 :: IO ()
main3 = 
    do
        putStr "Enter number: "
        hFlush stdout
        a <- getLine
        _ <- putStr "Enter number: "
        hFlush stdout
        b <- getLine
        putStrLn(show(read(a) + read(b)))

-- Read two args from input
main2 :: IO ()
main2 = 
    do
        args <- getArgs
        let a = read(args !! 0)
            b = read(args !! 1)
        putStrLn(show(a + b))

