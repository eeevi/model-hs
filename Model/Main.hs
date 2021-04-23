
-- | Main module.
module Model.Main (defaultMain) where

import Model.Core.Format
import Model.Core.Chain
import System.Random
import System.Environment


-- | Check command line args.
argiven :: [String] -> String
argiven [] = error "No input file in argumets!"
argiven xs = head xs


-- | Apply everything.
applicate :: String -> Chain -> String
applicate s c = concat $ map (\n -> model n c) (finalSplit s)



-- | Do some magic, again.
defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    base <- readFile $ argiven args
    c <- getStdGen

    let tenStates = setChain (take 10 (randoms c :: [Double]))
    let halfBase = tenStates 0.5

    putStrLn $ pretty $ applicate base $ unpack halfBase
