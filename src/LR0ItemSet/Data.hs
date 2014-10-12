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

newtype Terminal = Term Char
  deriving (Bounded, Enum, Eq, Ord)

instance Show Terminal where
    showsPrec _ (Term t) = showChar t

isTermChar :: Char -> Bool
isTermChar c = not $ any ($ c) [isUpper, (=='@'), (=='\'')]

newtype Nonterminal = Nonterm Char
  deriving (Bounded, Enum, Eq, Ord)

instance Show Nonterminal where
    showsPrec _ (Nonterm n) = showChar n

data Symbol = SymbolNonterm Nonterminal | SymbolTerm Terminal
  deriving (Eq, Ord)

instance Show Symbol where
    showsPrec k (SymbolNonterm n) = showsPrec k n
    showsPrec k (SymbolTerm t)    = showsPrec k t
    
    showList []     e = e
    showList (s:ss) e = shows s $ showList ss e

symbolToChar :: Symbol -> Char
symbolToChar (SymbolNonterm (Nonterm c)) = c
symbolToChar (SymbolTerm    (Term    c)) = c

type SentForm = [Symbol]

data Production = Production {
    pNonterm  :: Nonterminal
  , pSentForm :: SentForm
} deriving (Eq, Ord)

instance Show Production where
    showsPrec _ (Production lhs rhs) = shows lhs
                                     . showString "->"
                                     . shows rhs

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

dottify :: Production -> DotProduction
dottify (Production lhs rhs) = DotProduction lhs [] rhs

data Grammar = Grammar {
    gStartVariable :: Nonterminal
  , gProductions   :: [Production]
} deriving (Eq, Ord, Show)

data Items = Items {
    itemsNum         :: Word
  , itemsProductions :: [DotProduction]
} deriving (Eq, Ord, Show)

type LR0MonadWriter = MonadWriter Builder
type LR0Writer = Writer Builder

execLR0Writer :: LR0Writer a -> Text
execLR0Writer = toStrict . toLazyText . execWriter