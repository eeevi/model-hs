-- | Core.Format: module for formating.
module Core.Format where

import Data.Char (toUpper)


shuffle :: Ord a => [a] -> [a]
shuffle []       = []
shuffle [x]      = [x]
shuffle [x, y]   = [y, x]
shuffle (x:y:xs) = [y] ++ shuffle xs ++ [x]


symbol :: Char -> Bool
symbol n | n == '.' || n == '?' || n == '!' = True
         | otherwise = False 


pretty :: String -> String
pretty []     = []
pretty [x, y] = [x, y]
pretty (x:xs) | symbol x = [x] ++ space ++ rest
              | otherwise = [x] ++ pretty xs
                 where 
                    space = head xs : []
                    rest  = (toUpper $ head $ tail xs) : []
                            ++ pretty (tail $ tail xs)


finalSplit :: String -> [[String]]
finalSplit s = map words . shuffle $ lines s
