splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

firstTwo :: [a] -> (a, a)
firstTwo (h1:h2:_) = (h1, h2)
firstTwo _ = error "firstTwo of []"

parseLine :: String -> (Int, [Int])
parseLine s = let (target, lst) = firstTwo $ splitOnChar ':' s in
    (read target, fmap read $ words lst)

parse :: [String] -> [(Int, [Int])]
parse = fmap parseLine

(|||) :: Int -> Int -> Int
(|||) x y = read $ show x ++ show y

isSolvable :: Int -> (Int, [Int]) -> Bool
isSolvable acc (target, []) = acc == target
isSolvable acc (target, (h:t)) = isSolvable (acc + h) (target, t) 
                               || isSolvable (acc * h) (target, t) 
                               || isSolvable (acc ||| h) (target, t)

solve :: [(Int, [Int])] -> Int
solve l = foldl1 (+) $ fmap fst $ filter (isSolvable 0) l

main = do
    input <- readFile "j7/test.txt"
    print $ solve $ parse $ splitOnChar '\n' input