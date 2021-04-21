-- |
-- | Personal simple Markov chain model.
-- | Just for fun (and for practice, ofc.)

module Main where

import ModelM -- | Major functions defined here.
import System.Random


-- | Do some magic here.
main = do
    x <- getLine
    r <- readFile x
    let s = getAndSplit r
    n <- getStdGen

    let result = times s (take 10 (randoms n :: [Double]))
    putStrLn . conn $ result
