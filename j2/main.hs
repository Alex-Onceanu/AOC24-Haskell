splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

parse :: [String] -> [[Integer]]
parse lines = fmap ((fmap read) . words) lines

intOfBool :: Bool -> Integer
intOfBool b = if b then 1 else 0

cond :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
cond f x y = f x y && diff >= 1 && diff <= 3 where diff = abs(x - y)

forAllCouples :: (Integer -> Integer -> Bool) -> Bool -> [Integer] -> Bool
forAllCouples f b [] = True
forAllCouples f b (h:[]) = True
forAllCouples f b (h1:h2:t) = (cond f h1 h2 && forAllCouples f b (h2:t)) || (b && forAllCouples f False (h1:t))

modif :: (Integer -> Integer) -> [Integer] -> [Integer]
modif f [] = []
modif f (h:[]) = [h]
modif f (h1:h2:t) = (f h2):h1:h2:t

safe :: [Integer] -> Bool
safe l =    forAllCouples (<) True l 
        ||  forAllCouples (>) True l 
        ||  forAllCouples (<) True (modif (\x -> x - 1) l) 
        ||  forAllCouples (>) True (modif (\x -> x + 1) l)

solve :: [[Integer]] -> Integer
solve reports = foldl1 (+) $ fmap (intOfBool . safe) reports

main = do
    input <- readFile "j2/input.txt"
    print $ solve $ parse $ splitOnChar '\n' input
