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
    -- All operations in out language.
    ops = ["+","*","-","/",";","=",",","<",">","|",
          "||", "&&", ":", "==", "!=", ">=", "<="
          ]
    -- All keywords.
    names = ["_", "__", "___", "____", "_____"
            , "______", "extern", "binary"
            , "unary"
            ]
    style = emptyDef {
               Tok.commentLine = "#", -- Comments will start with '#'.
               Tok.reservedOpNames = ops,
               Tok.reservedNames = names
            }

-- These are all the lexers that we need for our language.
integer         = Tok.integer lexer
float           = Tok.float lexer
parens          = Tok.parens lexer
commaSep        = Tok.commaSep lexer
semiSep         = Tok.semiSep lexer
identifier      = Tok.identifier lexer
whitespace      = Tok.whiteSpace lexer
reserved        = Tok.reserved lexer
reservedOp      = Tok.reservedOp lexer
esotericInteger = esotericInteger_ lexer

operator :: Parser String
operator = do
  a <- Tok.opStart emptyDef
  b <- many $ Tok.opLetter emptyDef
  return (a:b)

-- Custom lexer for esoteric integers.
esotericInteger_ l = Tok.lexeme l int3 <?> "integer3"
  where int3 :: (Stream s m Char) => ParsecT s u m Integer
        int3 = number 3 dig3
        -- This will interpret the number as base tree and transform it.
        number base baseDigit
        -- Using many1 to ensure that we at least have one base3 digit to continue.
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

