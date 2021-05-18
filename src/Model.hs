-- | Model: main module.
module Model (defaultMain) where

import Core.Format
import Core.Chain
import System.Random (getStdGen, randoms)
import System.Environment (getArgs)


argiven :: [String] -> String
argiven [] = error "No input file in argumets!"
argiven xs = head xs


applicate :: String -> Chain -> String
applicate s c = concat $ map (\n -> model n c) (finalSplit s)


defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    base <- readFile $ argiven args
    c <- getStdGen

    let tenStates = setChain (take 20 (randoms c :: [Double]))
    let halfBase = tenStates 0.5

    putStrLn $ pretty $ applicate base $ unpack halfBase
