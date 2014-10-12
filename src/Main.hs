{-
Constructs the set of LR(0) parse table items from a deterministic context-free
grammar. This was created as Project 2 for EECS 665 at the University of Kansas.

Author: Ryan Scott
-}

module Main (main) where

import Data.Text.IO (putStr)

import LR0ItemSet.Algorithm
import LR0ItemSet.Data
import LR0ItemSet.Parse

import Prelude hiding (putStr)

import Text.Parsec (parse)

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