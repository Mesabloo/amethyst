{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeApplications, TemplateHaskell #-}

module Amethyst.Language.Parser where

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaC
import qualified Text.Megaparsec.Char.Lexer as MegaL
import qualified Data.Text as Text
import Data.Void
import Control.Lens

type Parser = Mega.Parsec Void Text.Text

data Expr
    = EAtom Lit
    | EId (Maybe Char) Text.Text
    | EBlock [Expr]
    deriving Show

data Lit
    = EInt Integer
    | EFloat Double
    | EString Text.Text
    | EChar Char
    deriving Show
makePrisms ''Lit

-------------------------------------------------------------------

program :: Parser [Expr]
program = lexeme (Mega.many expression) <* Mega.eof

expression :: Parser Expr
expression = lexeme $ Mega.choice
    [ EAtom . EChar <$> pChar
    , EAtom . EString <$> pString
    , EAtom . EFloat <$> Mega.try pFloat
    , EAtom . EInt <$> pInteger
    , EId <$> Mega.optional (char '\\') <*> pId
    , EId <$> Mega.optional (char '\\') <*> pOperator
    , EBlock <$> (char '{' *> Mega.many expression <* char '}') ]

pChar :: Parser Char
pChar = MegaC.char '\'' *> Mega.anySingle <* MegaC.char '\''

pString :: Parser Text.Text
pString = MegaC.char '"' *> Mega.takeWhileP Nothing (/= '"') <* MegaC.char '"'

pFloat :: Parser Double
pFloat = MegaL.float

pInteger :: Parser Integer
pInteger = MegaL.decimal

pId :: Parser Text.Text
pId = Text.pack <$> ((:) <$> MegaC.lowerChar <*> Mega.many MegaC.alphaNumChar)

pOperator :: Parser Text.Text
pOperator = Text.pack <$> Mega.some (MegaC.symbolChar Mega.<|> Mega.oneOf @[] "!#$%&.<=>?^~|@*/-+:")

-------------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme = MegaL.lexeme (MegaL.space MegaC.space1 lineCmnt blockCmnt)

lineCmnt :: Parser ()
lineCmnt = MegaL.skipLineComment "`"

blockCmnt :: Parser ()
blockCmnt = MegaL.skipBlockComment "(" ")"

char :: Char -> Parser Char
char = lexeme . MegaC.char

------------------------------------------------------------------