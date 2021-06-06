-- | Core.Chain: core chain module.
module Core.Chain
    ( model
    , setChain
    , Chain
    ) where


data Chain = Chain Chances
type Chances = [Double]


instance Show Chain where
    show (Chain c) = "Chain chances: " ++ show c


setChain :: [Double] -> Maybe Chain
setChain x | length x <= 2 = Nothing
           | otherwise     = Just (Chain x)


modelPredicate :: (Double, Double) -> String -> String
modelPredicate (x,y) s = case x >= y of
                          True  -> s ++ " "
                          False -> []


model :: [String] -> Chain -> String
model s (Chain x) = concat $ zipWith modelPredicate (zip x $ tail x) s 
