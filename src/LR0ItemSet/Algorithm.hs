{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module LR0ItemSet.Algorithm where

import           Control.Monad
import           Control.Monad.Writer

import qualified Data.Set as S
import           Data.Text.Lazy.Builder

import           LR0ItemSet.Data

tellLn :: LR0MonadWriter m => Builder -> m ()
tellLn = tell . (<> singleton '\n')

lr0ItemSet :: LR0MonadWriter m => Grammar -> m ()
lr0ItemSet g@(Grammar sv ps) = do
    tellLn "Augmented Grammar"
    tellLn "-----------------"
    
    let g'@(Grammar _ ps') = augment g
    forM_ ps' $ tellLn . fromString . show
    tellLn ""
    
    
    return ()

augment :: Grammar -> Grammar
augment (Grammar sv ps) = Grammar sv $ Production (Nonterm '\'') [SymbolNonterm sv]:ps

closure :: Grammar -> [DotProduction] -> [DotProduction]
closure g@(Grammar _ gps) jps =
    let jps' = jps ++ [ dotGp
                      | DotProduction _ _ (SymbolNonterm b:_) <- jps
                      , Production b' gamma <- gps
                      , b == b'
                      , let dotGp = DotProduction b' [] gamma
                      , not $ dotGp `elem` jps
                      ]
     in if jps == jps' then jps' else closure g jps'

goto :: Grammar -> [DotProduction] -> Symbol -> [DotProduction]
goto g i x = closure g [ DotProduction a (alpha ++ [x]) beta
                       | DotProduction a alpha (x':beta) <- i
                       , x == x'
                       ]