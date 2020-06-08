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

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "_____"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "______"
  cond <- expr
  reserved "_"
  tr <- expr
  reserved "_"
  fl <- expr
  return $ If cond tr fl

for :: Parser Expr
for = do
  reserved "__"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "__"
  body <- expr
  return $ For var start cond step body

while :: Parser Expr
while = do
  reserved "___"
  cond <- expr
  reserved "__"
  body <- expr
  return $ While cond body
  
letins :: Parser Expr
letins = do
  reserved "____"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "__"
  body <- expr
  return $ foldr (uncurry Let) body defs

unarydef :: Parser Expr
unarydef = do
  reserved "_____"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ UnaryDef o args body

binarydef :: Parser Expr
binarydef = do
  reserved "_____"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many identifier
  body <- expr
  return $ BinaryDef o args body

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
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
