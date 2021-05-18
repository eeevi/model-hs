
-- | Core.Chain: core chain module.
module Core.Chain where


data Chain = Chain Chances Current

type Chances = [Double]
type Current = Double


instance Show Chain where
    show (Chain x y) = show x ++ " " ++ show y


setChain :: [Double] -> Double -> Maybe Chain
setChain x y | x == []        = Nothing
             | otherwise      = Just (Chain x y)


unpack :: Maybe a -> a
unpack (Just n) = n
unpack Nothing  = error "Can't unpack Nothing."


model :: [String] -> Chain -> String
model x (Chain y z) | x == [] || y == [] = []
model (x:xs) (Chain (y:ys) z) | z >= y = x ++ " " ++ next
                              | otherwise = next
                                 where
                                 next = model xs (Chain ys y)

