
-- | Chain datatype personal module.
module Model.Core.Chain where

-- | Chain datatype with chances and current state.
data Chain = Chain Chances Current

-- | Just synonymous.
type Chances = [Double]
type Current = Double

-- | Pretty show for Chain datatype.
instance Show Chain where
    show (Chain x y) = show x ++ " " ++ show y


-- | Set chain with some current state.
setChain :: [Double] -> Double -> Maybe Chain
setChain x y | x == []        = Nothing
             | y > 1 || y < 0 = Nothing
             | otherwise      = Just (Chain x y)


-- | Unpack Maybe value just in case.
unpack :: Maybe a -> a
unpack (Just n) = n
unpack Nothing  = error "Can't unpack Nothing. :|"


-- | Building model here.
model :: [String] -> Chain -> String
model x (Chain y z) | x == [] || y == [] = []
model (x:xs) (Chain (y:ys) z) | z >= y = x ++ " " ++ next
                              | otherwise = next
                                 where
                                 next = model xs (Chain ys y)

