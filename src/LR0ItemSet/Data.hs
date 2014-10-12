{-
The data structures used for constructing the set of LR(0) items from a DCFG.
This was created as Project 2 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}

{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving #-}
module LR0ItemSet.Data (
    Terminal(..)
  , isTermChar
  , Nonterminal(..)
  , Symbol(..)
  , symbolToChar
  , SentForm
  , Production(..)
  , DotProduction(..)
  , dottify
  , Grammar(..)
  , Items(..)
  , LR0MonadWriter
  , LR0Writer
  , execLR0Writer
  ) where

import Control.Monad.Writer

import Data.Char
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Word

-- | A non-uppercase, non-@, non-' symbol (e.g., i).
newtype Terminal = Term Char
  deriving (Bounded, Enum, Eq, Ord)

instance Show Terminal where
    showsPrec _ (Term t) = showChar t

-- | Can this character reprent a Terminal?
isTermChar :: Char -> Bool
isTermChar c = not $ any ($ c) [isUpper, (=='@'), (=='\'')]

-- | An uppercase symbol (e.g., E).
newtype Nonterminal = Nonterm Char
  deriving (Bounded, Enum, Eq, Ord)

instance Show Nonterminal where
    showsPrec _ (Nonterm n) = showChar n

-- A single letter in a sentential form (i.e., either a Terminal or a Nonterminal).
data Symbol = SymbolNonterm Nonterminal | SymbolTerm Terminal
  deriving (Eq, Ord)

instance Show Symbol where
    showsPrec k (SymbolNonterm n) = showsPrec k n
    showsPrec k (SymbolTerm t)    = showsPrec k t
    
    showList []     e = e
    showList (s:ss) e = shows s $ showList ss e

-- | Extracts the character representing a Symbol.
symbolToChar :: Symbol -> Char
symbolToChar (SymbolNonterm (Nonterm c)) = c
symbolToChar (SymbolTerm    (Term    c)) = c

-- | The right-hand side of a production.
type SentForm = [Symbol]

-- | A DCFG production (e.g., E->E+F).
data Production = Production {
    pNonterm  :: Nonterminal
  , pSentForm :: SentForm
} deriving (Eq, Ord)

instance Show Production where
    showsPrec _ (Production lhs rhs) = shows lhs
                                     . showString "->"
                                     . shows rhs

-- | A DCFG production with a dot on the right-hand side (e.g., E->E@+F).
data DotProduction = DotProduction {
    dpNonterm     :: Nonterminal
  , dpPreDotForm  :: SentForm
  , dpPostDotForm :: SentForm
} deriving (Eq, Ord)

instance Show DotProduction where
    showsPrec _ (DotProduction lhs preRhs postRhs) = shows lhs
                                                   . showString "->"
                                                   . shows preRhs
                                                   . showChar '@'
                                                   . shows postRhs

-- | Places a dot in front of the right-hand side of a DCFG production.
--   (e.g., E->E+F to E->@E+F).
dottify :: Production -> DotProduction
dottify (Production lhs rhs) = DotProduction lhs [] rhs

-- | A deterministic context-free grammar.
data Grammar = Grammar {
    gStartVariable :: Nonterminal
  , gProductions   :: [Production]
} deriving (Eq, Ord, Show)

-- | A set of DCFG productions, labeled with a nonnegative integer.
data Items = Items {
    itemsNum         :: Word
  , itemsProductions :: [DotProduction]
} deriving (Eq, Ord, Show)

-- | Specialized Writer monad types used for logging while constructing the set
--   of LR(0) parse table items.
type LR0MonadWriter = MonadWriter Builder
type LR0Writer = Writer Builder

-- | Extract the logging output from an LR0Writer.
execLR0Writer :: LR0Writer a -> Text
execLR0Writer = toStrict . toLazyText . execWriter