{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving #-}
module LR0ItemSet.Data where

import Control.Monad.Writer

import Data.Char
import Data.Set (Set)
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

-- terminal :: Char -> Terminal
-- terminal c | isTermChar c = Term c
-- terminal _ = error "Terminals cannot be uppercase letters, at signs, or single quotes."

newtype Nonterminal = Nonterm Char
  deriving (Bounded, Enum, Eq, Ord)

instance Show Nonterminal where
    showsPrec _ (Nonterm n) = showChar n

-- nonterminal :: Char -> Nonterminal
-- nonterminal c | isUpper c = Nonterm c
-- nonterminal _ = error "Nonterminals must be uppercase letters."

data Symbol = SymbolNonterm Nonterminal | SymbolTerm Terminal
  deriving (Eq, Ord)

instance Show Symbol where
    showsPrec k (SymbolNonterm n) = showsPrec k n
    showsPrec k (SymbolTerm t)    = showsPrec k t
    
    showList []     e = e
    showList (s:ss) e = shows s $ showList ss e

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

-- ppProduction :: Production -> Builder
-- ppProduction (lhs, rhs) = fromString (show lhs)
--                        <> "->"
--                        <> fromString (concatMap show rhs)

-- data ItemSet = ItemSet {
--     itemSetNum :: Word
--   , itemSet    :: Set [DotProduction]
-- } deriving (Eq, Ord, Show)

data Grammar = Grammar {
    gStartVariable :: Nonterminal
  , gProductions   :: [Production]
} deriving (Eq, Ord, Show)

data Items = Items {
    itemsNum         :: Word
  , itemsProductions :: [Production]
} deriving (Ord, Show)

instance Eq Items where
    (Items _ ips1) == (Items _ ips2) = ips1 == ips2

-- data DotGrammar = DotGrammar {
--     dgStartVariable :: Nonterminal
--   , dgProductions   :: [DotProduction]
-- } deriving (Eq, Ord, Show)

-- dottify :: Grammar -> DotGrammar
-- dottify (Grammar sv ps) = DotGrammar sv $ map (\(lhs, rhs) -> (lhs, [], rhs)) ps

type LR0MonadWriter = MonadWriter Builder
type LR0Writer = Writer Builder

runLR0Writer :: LR0Writer a -> (a, Text)
runLR0Writer = fmap (toStrict . toLazyText) . runWriter

execLR0Writer :: LR0Writer a -> Text
execLR0Writer = toStrict . toLazyText . execWriter