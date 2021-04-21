-- |
-- | Something really cringe.

module ModelM where

import Control.Monad (replicateM)
import System.Random

-- | Chain type with two states.
data Chain = Chain Prev Current

-- | Previous and current chain states.
type Prev    = Double
type Current = Double

-- | Pretty show.
instance Show Chain where
    show (Chain p c) = "p: " ++ show p ++ "\n" ++ "c: " ++ show c

-- | Num class instanses.
instance Num Chain where
    (+) (Chain a b) (Chain c d) = Chain (a + c) (b + d)
    (*) (Chain a b) (Chain c d) = Chain (a * b) (c * d)
    fromInteger 0               = Chain 0 0
    fromInteger 1               = Chain 0 1
    fromInteger n               = (Chain 0 1) + fromInteger (n - 1)

    -- | Just to avoid warnings while compile.
    abs (Chain a b)    = Chain a b
    negate (Chain a b) = Chain (-a) (-b)
    signum (Chain a b) = 1


-- | Some split function. 
splitt :: Eq a => a -> [a] -> [[a]]
splitt x y = case break (== x) y of
            (xs, []) -> [xs]
            (xs, l:ls) -> [xs] ++ splitt x ls


-- | Some 2-depth list pseudo-shuffle.
llsh :: [[a]] -> [[a]]
llsh []       = []
llsh [[x]]    = [[x]]
llsh [x, y]   = [x, y]
llsh (x:y:xs) = [y] ++ llsh xs ++ [x] 


-- | Main thing, yes. :>
model :: [String] -> Chain -> [Double] -> String
model x _ y | x == [] || y == [] = []
model (x:y) (Chain p c) (g:l) = let mNext = model y (Chain c g) l
                                in case c >= g of
                                    True  -> x ++ " " ++ mNext
                                    False -> mNext


-- | Will split whole String into sentences by '\n'.
-- | Then will split every sentence by ' '.
getAndSplit :: String -> [[String]]
getAndSplit x = map (splitt ' ') (llsh (splitt '\n' x))


-- | Build model with 0.5 start statement.
applyWithHalf :: [String] -> [Double] -> String
applyWithHalf x y = model x (Chain 0.0 0.2) y


-- | Apply model to some data with some chances.
times :: [[String]] -> [Double] -> [String]
times y chances = map (\n -> applyWithHalf n chances) y


-- | Will connect list of strings to one single.
conn :: [String] -> String
conn [] = []
conn (x:xs) = x ++ conn xs
