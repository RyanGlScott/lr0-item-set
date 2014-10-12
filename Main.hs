{-
Constructs the set of LR(0) parse table items from a deterministic context-free
grammar. This was created as Project 2 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}

{-# LANGUAGE ConstraintKinds, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Main (main) where

import           Control.Applicative hiding ((<|>))
import           Control.Monad
import           Control.Monad.Writer

import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Text.IO (putStr)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Word

import           Prelude hiding (putStr)

import           Text.Parsec
import           Text.Parsec.String
import           Text.Printf

-- | Where the program begins
main :: IO ()
main = do
    -- Parse standard input
    stuff <- getContents
    case parse parseGrammar "" stuff of
         -- If unsuccessful, abort with an error message.
         Left e  -> print e
         -- Otherwise, find the LR(0) item set and print the results.
         Right g -> putStr . execLR0Writer $ lr0ItemSet g

---------------------------------------------------------------------------------------------------
-- The algorithms used for constructing the set of LR(0) items from a DCFG.
---------------------------------------------------------------------------------------------------

-- | Constructs the set of LR(0) parse table items from a DCFG.
lr0ItemSet :: LR0MonadWriter m => Grammar -> m (Set Items)
lr0ItemSet g = do
    tellLn "Augmented Grammar"
    tellLn "-----------------"
    
    -- First, augment the grammar with a new start production '->S.
    let g'@(Grammar _ ps') = augment g
    
    -- Log the augmented grammar.
    forM_ ps' $ tellLn . fromString . show
    tellLn ""
    
    -- Compute C = closure({['->@S]}).
    let i0 = closure g' [dottify $ head ps']
        c  = S.singleton $ Items 0 i0
    -- Pass C to the "for-loop".
    itemize g' c

-- | Do some simple logging before doing the real work in itemize'.
itemize :: LR0MonadWriter m => Grammar -> Set Items -> m (Set Items)
itemize g c = do
    tellLn "Sets of LR(0) Items"
    tellLn "-------------------"
    
    itemize' g c c

-- | This represents the for-loops in the pseudocode representation of the algorithm.
--   In this implementation, a set is used to track newly added sets of items; when
--   it is empty, the for-loop has ended.
itemize' :: LR0MonadWriter m => Grammar -> Set Items -> Set Items -> m (Set Items)
itemize' g c unmarked | S.null unmarked = return c -- If no unmarked states, we're done.
                      | otherwise       = do       -- Otherwise, go through unmarked states.
        -- Find the item set with the smallest numeric label...
    let i@(Items n ps) = S.findMin unmarked
        -- ...then remove it from the unmarked set.
        unmarked'      = S.delete i unmarked
        
    tellLn $ singleton 'I' <> decimal n <> singleton ':'
    
    -- Compute the grammar symbols immediately following dots (in the order in which
    -- they appear in the item set).
    let xs = nub [x | DotProduction _ _ (x:_) <- ps]
        
    -- Log each dotted production, as well as its corresponding goto (if it has not
    -- been logged previously in the function call).
    (c', unmarked'', _) <- forFoldM (c, unmarked', xs) ps $ \(c', unmarked'', xs') p@(DotProduction _ _ rhs) -> do
        tell "    "
        
        -- If there is a post-dot grammar symbol matching the start of this
        -- production's post-dot right-hand side...
        if not (null rhs) && head rhs `elem` xs'
           then do -- ...then calculate its goto (since it won't be empty)...
               let symb            = head rhs
                   oldMax          = itemsNum $ S.findMax c' -- The largest current item set label.
                   newMax          = oldMax + 1 -- The label to give the new item set.
                   gotoItemSet     = Items newMax $ goto g ps symb -- The goto.
               -- If the goto is the items set...
               if inItemsSet gotoItemSet c
                  then do -- ...then indicate which items are already in the set.
                      let n' = itemsNum $ lookupItemsSet gotoItemSet c
                      tellLn . fromString $ printf "%-20s goto(%c)=I%d" (show p) (symbolToChar symb) n'
                      return (c', unmarked'', delete symb xs')
                  else do -- ...otherwise, give the items a new label.
                      tellLn . fromString $ printf "%-20s goto(%c)=I%d" (show p) (symbolToChar symb) newMax
                      return $ (S.insert gotoItemSet c', S.insert gotoItemSet unmarked'', delete symb xs')
           else do -- ... otherwise, just print out the production.
               tellLn . fromString $ show p
               return (c', unmarked'', xs')
    tellLn ""
    itemize' g c' unmarked''

-- | Log some output followed by a newline.
tellLn :: LR0MonadWriter m => Builder -> m ()
tellLn = tell . (<> singleton '\n')

-- | A more convenient form of 'foldM' for use with do-notation.
forFoldM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
forFoldM = flip . flip foldM

-- | Checks if some LR(0) items are in a set, ignoring labels.
inItemsSet :: Items -> Set Items -> Bool
inItemsSet (Items _ ps) = any ((==ps) . itemsProductions) . S.toList

-- | Returns the member of a set of LR(0) items that matches the input items,
--   ignoring labels.
lookupItemsSet :: Items -> Set Items -> Items
lookupItemsSet (Items _ ps) = fromJust . find ((==ps) . itemsProductions) . S.toList

-- | Attaches a new start production ('->S) to a grammar, where S is the start symbol.
augment :: Grammar -> Grammar
augment (Grammar sv ps) = Grammar sv $ Production (Nonterm '\'') [SymbolNonterm sv]:ps

-- | Computes the dot-closure of a set of dotted DCFG productions.
closure :: Grammar -> [DotProduction] -> [DotProduction]
closure g jps =
    let jps' = jps ++ [ dotGp
                      | DotProduction _ _ (SymbolNonterm b:_) <- jps
                      , Production b' gamma <- gProductions g
                      , b == b'
                      , let dotGp = DotProduction b' [] gamma
                      , not $ dotGp `elem` jps
                      ]
     in if jps == jps'
           then jps'
           else closure g jps'

-- | Computes the dotted productions that result from shifting the dots preceding
--   a certain symbol over by one (as well as their closure).
goto :: Grammar -> [DotProduction] -> Symbol -> [DotProduction]
goto g i x = closure g [ DotProduction a (alpha ++ [x]) beta
                       | DotProduction a alpha (x':beta) <- i
                       , x == x'
                       ]

---------------------------------------------------------------------------------------------------
-- The data structures used for constructing the set of LR(0) items from a DCFG.
---------------------------------------------------------------------------------------------------

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
data Production = Production Nonterminal SentForm
  deriving (Eq, Ord)

instance Show Production where
    showsPrec _ (Production lhs rhs) = shows lhs
                                     . showString "->"
                                     . shows rhs

-- | A DCFG production with a dot on the right-hand side (e.g., E->E@+F).
data DotProduction = DotProduction Nonterminal SentForm SentForm
  deriving (Eq, Ord)

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
    _gStartVariable :: Nonterminal
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

---------------------------------------------------------------------------------------------------
-- The parser combinators used when reading in input as a DCFG.
---------------------------------------------------------------------------------------------------

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