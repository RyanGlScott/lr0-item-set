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

lr0ItemSet :: LR0MonadWriter m => Grammar -> m (Set Items)
lr0ItemSet g = do
    tellLn "Augmented Grammar"
    tellLn "-----------------"
    
    let g'@(Grammar _ ps') = augment g
    forM_ ps' $ tellLn . fromString . show
    tellLn ""
    
    let i0 = closure g' [dottify $ head ps']
        c  = S.singleton $ Items 0 i0
    itemize g' c

itemize :: LR0MonadWriter m => Grammar -> Set Items -> m (Set Items)
itemize g c = do
    tellLn "Sets of LR(0) Items"
    tellLn "-------------------"
    itemize' g c c

itemize' :: LR0MonadWriter m => Grammar -> Set Items -> Set Items -> m (Set Items)
itemize' g c unmarked | S.null unmarked = return c
                      | otherwise       = do
    let i@(Items n ps) = S.findMin unmarked
        unmarked'      = S.delete i unmarked
    tellLn $ singleton 'I' <> decimal n <> singleton ':'
    let xs = nub [x | DotProduction _ _ (x:_) <- ps]
    (c', unmarked'', _) <- forFoldM (c, unmarked', xs) ps $ \(c', unmarked'', xs') p@(DotProduction _ _ rhs) -> do
        tell "    "
        if not (null rhs) && head rhs `elem` xs'
           then do
               let symb = head rhs
                   oldMax = itemsNum $ S.findMax c'
                   newMax = oldMax + 1
                   got    = Items newMax $ goto g ps symb
               if inItemsSet got c
                  then do
                      let n' = itemsNum $ lookupItemsSet got c
                      tellLn . fromString $ printf "%-20s goto(%c)=I%d" (show p) (symbolToChar symb) n'
                      return (c', unmarked'', delete symb xs')
                  else do
                      tellLn . fromString $ printf "%-20s goto(%c)=I%d" (show p) (symbolToChar symb) newMax
                      return $ (S.insert got c', S.insert got unmarked'', delete symb xs')
           else do
               tellLn . fromString $ show p
               return (c', unmarked'', xs')
    tellLn ""
    itemize' g c' unmarked''

tellLn :: LR0MonadWriter m => Builder -> m ()
tellLn = tell . (<> singleton '\n')

forFoldM :: Monad m => a -> [b] -> (a -> b -> m a) -> m a
forFoldM = flip . flip foldM

inItemsSet :: Items -> Set Items -> Bool
inItemsSet (Items _ ps) = any ((==ps) . itemsProductions) . S.toList

lookupItemsSet :: Items -> Set Items -> Items
lookupItemsSet (Items _ ps) = fromJust . find ((==ps) . itemsProductions) . S.toList

augment :: Grammar -> Grammar
augment (Grammar sv ps) = Grammar sv $ Production (Nonterm '\'') [SymbolNonterm sv]:ps

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

goto :: Grammar -> [DotProduction] -> Symbol -> [DotProduction]
goto g i x = closure g [ DotProduction a (alpha ++ [x]) beta
                       | DotProduction a alpha (x':beta) <- i
                       , x == x'
                       ]