-- | Core.Format: module for formating.
module Core.Format 
    ( space
    , finalSplit 
    ) where

type Text = [String]


shuffle :: Ord a => [a] -> [a]
shuffle x | length x <= 2 = x
shuffle (x:y:xs) = [y] ++ shuffle xs ++ [x]

-- spaces between two words.
space :: Text -> Text
space t = zipWith (++) (filter (\s -> s /= []) t) spaces
          where spaces = take (length t) (repeat " ")


finalSplit :: String -> [[String]]
finalSplit s = map words . shuffle $ lines s
