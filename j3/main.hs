import Text.Regex.Posix

splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

prod :: (Integer, Integer) -> Integer
prod tup = fst tup * snd tup

cropEnd :: String -> String
cropEnd [] = []
cropEnd (h:[]) = []
cropEnd (h:t) = h:(cropEnd t)

crop :: String -> String
crop [] = []
crop (h:t) = cropEnd t

tupleOfList :: [Integer] -> (Integer, Integer)
tupleOfList [] = (0, 0)
tupleOfList (a:b:t) = (a, b)
tupleOfList (h:t) = (h, 0)

findNumbers :: String -> (Integer, Integer)
findNumbers "" = (0, 0)
findNumbers s = tupleOfList $ fmap read $ splitOnChar ',' s'
    where s' = crop $ snd $ break (== '(') s

compute :: String -> Integer
compute = prod . findNumbers

shouldDo :: Bool -> [Bool] -> Bool
shouldDo init [] = init
shouldDo init sl = foldl1 (\x y -> y) sl

-- regexMatches s = getAllTextMatches (s =~ "mul\\([0-9]+,[0-9]+\\)" :: AllTextMatches [] String)
regexMatches :: Bool -> String -> [String]
regexMatches set "" = []
regexMatches set s =
    let (b, x, a, _) = s =~ "mul\\([0-9]+,[0-9]+\\)" :: (String, String, String, [String]) in
        let doDonts = getAllTextMatches (b =~ "do\\(\\)|don't\\(\\)" :: AllTextMatches [] String) in 
            let set' = shouldDo set $ fmap (\s -> if s == "do()" then True else if s == "don't()" then False else error "helo") doDonts in
                if set' then x : (regexMatches set' a)
                else regexMatches set' a


solveLine :: String -> Integer
solveLine s = foldl1 (+) $ fmap compute $ regexMatches True s

-- solve :: [String] -> Integer
-- solve sl = foldl1 (+) $ fmap solveLine sl

main = do
    input <- readFile "j3/input2.txt"
    print $ solveLine input
    -- print $ solve $ splitOnChar '\n' input
    -- print $ regexMatches True input