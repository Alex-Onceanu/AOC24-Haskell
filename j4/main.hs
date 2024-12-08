import Data.List

-- fmapi :: (Integer -> a -> b) -> [a] -> [b]
-- fmapi f [] = []
-- fmapi f (h:t) = (f h) : (fmapi f t)

splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

(+++) :: (a -> Integer) -> (a -> Integer) -> (a -> Integer)
(+++) f1 f2 = \x -> f1 x + f2 x

nbOccurencesLine :: String -> String -> Integer
nbOccurencesLine pattern [] = 0
nbOccurencesLine pattern (h:t) = (if isPrefixOf pattern (h:t) then 1 else 0) + nbOccurencesLine pattern t

nbOccurences :: String -> [String] -> Integer
nbOccurences pattern [] = 0
nbOccurences pattern sl = foldl1 (+) $ fmap (nbOccurencesLine pattern) sl

cycleList :: [[c]] -> [[c]]
cycleList [] = []
cycleList (h:t) = t ++ [h]

splitOnLength :: Int -> [[c]] -> ([[c]], [[c]])
splitOnLength n [] = ([], [])
splitOnLength 0 sl = ([], sl)
splitOnLength n (h:t) = let (before, after) = splitOnLength (n - 1) t in (h:before, after)

diagLine :: [[c]] -> [c] -> [[c]]
diagLine acc sl = (fmap (\tup -> (fst tup) : (snd tup)) $ zip sl before) ++ after
    where (before, after) = splitOnLength (length sl) acc

diag :: [[c]] -> [[c]] -> [[c]]
diag acc [] = acc
diag acc (line:sl) = diag (cycleList $ diagLine acc line) sl

initList :: Int -> a -> [a]
initList 0 x = []
initList n x = x:(initList (n - 1) x)

transposeAll :: [[c]] -> [[[c]]]
transposeAll l = l:(transpose l):(diag acc l):(diag acc (fmap reverse l)):[] 
    where acc = initList ((length l) + (length $ head l) - 1) []
    -- where acc = (fmap (\s -> []) $ head l)

solve :: String -> [String] -> Integer
solve pat sl = foldl1 (+) $ fmap ((nbOccurences pat) +++ (nbOccurences (reverse pat))) $ transposeAll sl

everyXOfZipped :: [(Char, Char, Char)] -> [(String, String, String)]
everyXOfZipped [] = []
everyXOfZipped (tupl:next) = 
    case firstThree (tupl:next) of
        Nothing -> []
        Just (tup1, tup2, tup3) -> (unzip3 [tup1, tup2, tup3]) : (everyXOfZipped next)

everyXOfLine :: [String] -> [(String, String, String)]
everyXOfLine sl = 
    case firstThree sl of
        Nothing -> []
        Just (s1, s2, s3) -> everyXOfZipped $ zip3 s1 s2 s3

firstThree :: [a] -> Maybe (a, a, a)
firstThree (h1:h2:h3:_) = Just (h1, h2, h3)
firstThree _ = Nothing

everyX :: [String] -> [(String, String, String)]
everyX [] = []
everyX (h:t) = (everyXOfLine (h:t)) ++ everyX t

extractX :: (String, String, String) -> (Char, Char, Char, Char, Char)
extractX (s1, s2, s3) = (s1 !! 0, s1 !! 2, s2 !! 1, s3 !! 0, s3 !! 2)

tryAgain :: ((Char, Char, Char, Char, Char) -> Bool) -> (Char, Char, Char, Char, Char) -> Bool
tryAgain f = \(a, b, c, d, e) -> f (a, b, c, d, e) || f (a, d, c, b, e)

isXMAS :: (Char, Char, Char, Char, Char) -> Bool
isXMAS (a, b, c, d, e) =    c == 'A' 
                        && (a == 'M' || a == 'S') 
                        &&  a == b
                        && (d == 'M' || d == 'S') 
                        &&  d == e
                        &&  a /= d

solvePart2 :: [String] -> Int
solvePart2 sl = length $ filter ((tryAgain isXMAS) . extractX) $ everyX sl

main = do
    input <- readFile "j4/input.txt"
    -- print $ solve "XMAS" $ splitOnChar '\n' input
    -- print $ diag [[],[],[],[],[],[]] [[1, 2, 3], [4, 5, 6], [7 ,8, 9], [10, 11, 12]]
    -- print $ cycleList $ diagLine [[2],[3],[1]] [4, 5, 6]
    print $ solvePart2 $ splitOnChar '\n' input
    -- print $ isXMAS $ extractX $ head $ everyX $ splitOnChar '\n' input