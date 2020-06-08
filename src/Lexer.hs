{-# LANGUAGE FlexibleContexts #-}
module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many, ParsecT, Stream, tokenPrim, (<?>))
import Text.Parsec.Pos (setSourceLine, setSourceColumn, sourceColumn, sourceLine, SourcePos, updatePosString)
import Text.Parsec.Char (satisfy)
import Text.Parsec.Combinator (many1)
import Data.Char (digitToInt)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/",";","=",",","<",">","|"
          , ":", "==", "!=", ">=", "<="]
    names = ["_", "__", "___", "____", "_____"
            , "______", "extern", "binary"
            , "unary"]
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
esotericInteger = esotericInteger_ lexer

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c:cs)


esotericInteger_ l = Tok.lexeme l int3 <?> "integer3"
  where int3 :: (Stream s m Char) => ParsecT s u m Integer
        int3 = number 3 dig3
        number base baseDigit
          = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }
        dig3 :: (Stream s m Char) => ParsecT s u m Char
        dig3 = satisfy isBase3
        isBase3 '0' = True
        isBase3 '1' = True
        isBase3 '2' = True
        isBase3 _ = False

