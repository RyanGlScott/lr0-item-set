{-
The parser combinators used when reading in input as a DCFG.
This was created as Project 2 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}

module LR0ItemSet.Parse (parseGrammar) where

import Control.Applicative hiding ((<|>))

import Data.Char

import LR0ItemSet.Data

import Text.Parsec
import Text.Parsec.String

-- | Parses an uppercase letter as a nonterminal.
parseNonterminal :: Parser Nonterminal
parseNonterminal = Nonterm <$> satisfy isUpper

-- | Parses a non-uppercase, non-@, non-' character as a terminal.
parseTerminal :: Parser Terminal
parseTerminal = Term <$> satisfy isTermChar

-- | Parses either a nonterminal or a terminal.
parseSymbol :: Parser Symbol
parseSymbol = (SymbolTerm <$> try parseTerminal)
          <|> (SymbolNonterm <$> parseNonterminal)

-- | Parses a production (i.e., E->E+T or T->).
parseProduction :: Parser Production
parseProduction = Production
    <$> parseNonterminal
    <*  string "->"
    -- Read symbols until a newline is encountered
    <*> manyTill parseSymbol newline

-- | Parses an entire deterministic context-free grammar.
parseGrammar :: Parser Grammar
parseGrammar = Grammar
    <$> parseNonterminal
    <*  spaces
    <*> manyTill parseProduction (try $ spaces *> eof)