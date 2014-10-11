module Main (main) where

import Data.Text.IO (putStr)

import LR0ItemSet.Algorithm
import LR0ItemSet.Data
import LR0ItemSet.Parse

import Prelude hiding (putStr)

import Text.Parsec (parse)

main :: IO ()
main = do
    stuff <- getContents
    case parse parseGrammar "" stuff of
         Left e  -> print e
         Right g -> putStr . execLR0Writer $ lr0ItemSet g