
-- | Output and kinda like output formating.
module Model.Core.Format where

import Data.Char


-- | Some array shuffle function. 
shuffle :: Ord a => [a] -> [a]
shuffle []       = []
shuffle [x]      = [x]
shuffle [x, y]   = [y, x]
shuffle (x:y:xs) = [y] ++ shuffle xs ++ [x]


-- | Some format symbols.
symbol :: Char -> Bool
symbol n | n == '.' || n == '?' || n == '!' = True
         | otherwise = False 


-- | Pretify some string.
pretty :: String -> String
pretty []     = []
pretty [x, y] = [x, y]
pretty (x:xs) | symbol x = [x] ++ space ++ rest
              | otherwise = [x] ++ pretty xs
                 where 
                    space = head xs : []
                    rest  = (toUpper $ head $ tail xs) : []
                            ++ pretty (tail $ tail xs)

-- | Format for model.
finalSplit :: String -> [[String]]
finalSplit s = map words (shuffle $ lines s)
