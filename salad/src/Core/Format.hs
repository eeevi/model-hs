-- | Core.Format: module for formating.
module Core.Format 
    ( pretty
    , finalSplit 
    ) where

import Data.Char (toUpper)


shuffle :: Ord a => [a] -> [a]
shuffle x | length x <= 2 = x
shuffle (x:y:xs) = [y] ++ shuffle xs ++ [x]


symbol :: Char -> Bool
symbol n | n == '.' || n == '?' || n == '!' = True
         | otherwise = False 


pretty :: String -> String
pretty x | length x <= 2 = x
pretty (x:xs) | symbol x = [x] ++ space ++ rest
              | otherwise = [x] ++ pretty xs
                where space = head xs : []
                      tail' = tail xs
                      rest  = (toUpper . head $ tail') : []
                           ++ (pretty . tail $ tail')


finalSplit :: String -> [[String]]
finalSplit s = map words . shuffle $ lines s
