{-
The algorithms used for constructing the set of LR(0) items from a DCFG.
This was created as Project 2 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module LR0ItemSet.Algorithm (
    lr0ItemSet
  , augment
  , closure
  , goto
  ) where

import           Control.Monad
import           Control.Monad.Writer

import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int

import           LR0ItemSet.Data

import           Text.Printf

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
    let jps' = jps ++ nub [ dotGp
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
