module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as Char

import Lexer
import Syntax

int :: Parser Expr
int = Int <$> integer

-- Will try to parse an Int using base three. If it can't, it
-- will throw an 'unexpected' error so the parser can continue
-- trying with other options.
esotericInt :: Parser Expr
esotericInt = do
  esoteric <- esotericInteger
  otherInt <- optionMaybe $ Char.oneOf "3456789"
  case otherInt of
    Nothing -> return $ Int esoteric
    (Just _) -> do
      throwError <- unexpected "notBaseThree"
      return $ Int esoteric

floating :: Parser Expr
floating = Float <$> float

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s = Ex.Infix (reservedOp s >> return (BinaryOp s))

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

-- The order of the binary operators indicates their relative precedence.
binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft,
          binary ">" Ex.AssocLeft,
          binary "==" Ex.AssocLeft,
          binary "!=" Ex.AssocLeft,
          binary ">=" Ex.AssocLeft,
          binary "<=" Ex.AssocLeft,
          binary "&&" Ex.AssocLeft,
          binary "||" Ex.AssocLeft]]

-- Will build an expression parser with all the possible inputs but functions.
expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

-- Checks for a function to parse in the expected order
function :: Parser Expr
function = do
  reserved "_____" -- 5 undersores.
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

-- Checks for an external function and assigns an identifier to it.
extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

-- Checks for a call of a function.
call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

-- If then logic. Since it should always evaluate something, 
-- an else clause should always be present.
ifthen :: Parser Expr
ifthen = do
  reserved "______" -- 6 underscores.
  cond <- expr
  reserved "_" -- If: 1 underscore.
  tr <- expr
  reserved "_" -- Then: 1 underscore.
  fl <- expr
  return $ If cond tr fl

-- For logic. Parses all the expressions in each of the components of the 
-- for loop.
for :: Parser Expr
for = do
  reserved "__" -- 2 underscores.
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "__" -- 2 underscores.
  body <- expr
  return $ For var start cond step body

-- While logic. 
while :: Parser Expr
while = do
  reserved "___" -- 3 underscores.
  cond <- expr
  reserved "__" -- 2 underscores.
  body <- expr
  return $ While cond body
  
-- Variable definition logic. Can define multiple variables separated by a 
-- comma.
letins :: Parser Expr
letins = do
  reserved "____" -- 4 underscores.
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "__" -- 2 underscores.
  body <- expr
  return $ foldr (uncurry Let) body defs

-- Unary op definition for user.
unarydef :: Parser Expr
unarydef = do
  reserved "_____" -- 5 underscores
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ UnaryDef o args body

-- Binary op definition for user.
binarydef :: Parser Expr
binarydef = do
  reserved "_____" -- 5 underscores.
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many identifier
  body <- expr
  return $ BinaryDef o args body

-- Tries to parse using all the different inputs. If one fails, the next one
-- is tried.
factor :: Parser Expr
factor = try floating
      <|> try esotericInt
      <|> try int
      <|> try call
      <|> try variable
      <|> ifthen
      <|> try letins
      <|> for
      <|> while
      <|> parens expr

-- Tries to parse any of the function definitions (or exp). If one fails, the next one
-- is tried.
defn :: Parser Expr
defn = try extern
    <|> try function
    <|> try unarydef
    <|> try binarydef
    <|> expr

contents :: Parser a -> Parser a
contents parser = do
  Tok.whiteSpace lexer
  rParser <- parser
  eof
  return rParser

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";" -- Always expect a semicolon after an expression.
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
