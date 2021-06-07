-- | Model: main module.
module Model 
    ( buildText
    ) where

import Core.Format
import Core.Chain

import System.Environment (getArgs)
import System.Random      (getStdGen, randoms)


argiven :: [String] -> String
argiven [] = error "No input file in argumets!"
argiven xs = head xs

-- | for every line.
applicate :: String -> Chain -> [String]
applicate s c = map (\n -> concat . space $ model n c) (finalSplit s)


buildText :: IO ()
buildText = do
    args <- getArgs
    base <- readFile $ argiven args
    c <- getStdGen

    let tenStates = setChain $ take 30 (randoms c :: [Double])
    let result = concat <$> space <$> applicate base <$> tenStates

    case result of
        Nothing -> putStrLn "err, chain building failed."
        Just st -> putStrLn st 
