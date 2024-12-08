import Data.Array

len l = case l of 
    [] -> 0
    (h:t) -> 1 + len t

start :: [Integer] -> Integer
start l = foldl1 max l

parse :: String -> [Integer]
parse input = fmap read (words input)

createArray :: Integer -> (Integer -> a) -> Array Integer a
createArray n f = array (0, n - 1) [(i, f i) | i <- [0..(n-1)]]

createMatrix :: Integer -> Integer -> (Integer -> Integer -> a) -> Array Integer (Array Integer a)
createMatrix n m f = createArray n (\y -> createArray m $ f y)

main = do 
    input <- getLine
    -- print $ start $ parse $ n
    print $ createMatrix 3 3 (\x y -> x + y)