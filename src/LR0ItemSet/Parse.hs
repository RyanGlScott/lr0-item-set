module LR0ItemSet.Parse (parseGrammar) where

import Control.Applicative hiding ((<|>))

import Data.Char

import LR0ItemSet.Data

import Text.Parsec
import Text.Parsec.String

parseNonterminal :: Parser Nonterminal
parseNonterminal = Nonterm <$> satisfy isUpper

parseTerminal :: Parser Terminal
parseTerminal = Term <$> satisfy isTermChar

parseSymbol :: Parser Symbol
parseSymbol = (SymbolTerm <$> try parseTerminal)
          <|> (SymbolNonterm <$> parseNonterminal)

parseProduction :: Parser Production
parseProduction = Production
    <$> parseNonterminal
    <*  string "->"
    <*> manyTill parseSymbol newline

parseGrammar :: Parser Grammar
parseGrammar = Grammar
    <$> parseNonterminal
    <*  spaces
    <*> manyTill parseProduction (try $ spaces *> eof)