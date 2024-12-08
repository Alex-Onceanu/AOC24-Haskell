import Data.List

splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

parse :: [String] -> ([Integer], [Integer])
parse l = unzip $ fmap (\s -> let (h, t) = break (== ' ') s in (read h, let x : _ = words t in read x)) l

sumOfDiffs :: [Integer] -> [Integer] -> Integer
sumOfDiffs l1 l2 = foldl1 (+) $ zipWith (\x y -> abs (x - y)) l1 l2

solve :: ([Integer], [Integer]) -> Integer
solve tup = sumOfDiffs (sort (fst tup)) (sort (snd tup))

occurences :: [Integer] -> Integer -> Integer
occurences l v = foldl1 (+) $ fmap (\x -> if x == v then x else 0) l

solve2 :: ([Integer], [Integer]) -> Integer
solve2 tup = foldl1 (+) $ fmap (occurences $ snd tup) $ fst tup

main = do
    input <- readFile "j1/input.txt"
    print $ solve $ parse $ splitOnChar '\n' input