data Direction = Dright | Dup | Dleft | Ddown
type Position = (Int, Int)

intOfDir :: Direction -> Int
intOfDir Dright = 1
intOfDir Dup = 2
intOfDir Dleft = 3
intOfDir Ddown = 4

direq :: Direction -> Direction -> Bool
direq d1 d2 = intOfDir d1 == intOfDir d2

turnRight :: Direction -> Direction
turnRight Dright = Ddown
turnRight Dup = Dright
turnRight Dleft = Dup
turnRight Ddown = Dleft

myHead :: [a] -> a
myHead [] = error "head of []"
myHead (h:t) = h

assert :: Bool -> ()
assert b = if b then () else error "Assertion error"

fmapi :: (Int -> a -> b) -> Int -> [a] -> [b]
fmapi f i [] = []
fmapi f i (h:t) = (f i h) : (fmapi f (i + 1) t)

splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

findIndex :: (a -> Bool) -> Int -> [a] -> [Int]
findIndex cond i [] = []
findIndex cond i (h:t) = (if cond h then [i] else []) ++ findIndex cond (i + 1) t

findCharLine :: Char -> Int -> String -> [Position]
findCharLine c i s = fmap (\j -> (j, i)) $ findIndex (== c) 0 s

findChar :: Char -> [String] -> [Position]
findChar c sl = foldl1 (++) $ fmapi (findCharLine c) 0 sl

findGuard :: [String] -> (Position, Direction)
findGuard sl = (myHead $ findChar '^' sl, Dup)

parse :: [String] -> ([Position], (Position, Direction), (Int, Int))
parse sl = (findChar '#' sl, findGuard sl, (length $ myHead sl, length sl))

theoreticalNext :: (Position, Direction) -> (Position, Direction)
theoreticalNext ((x, y), Dright) = ((x + 1, y), Dright)
theoreticalNext ((x, y), Dup) = ((x, y - 1), Dup)
theoreticalNext ((x, y), Dleft) = ((x - 1, y), Dleft)
theoreticalNext ((x, y), Ddown) = ((x, y + 1), Ddown)

outOfBounds :: (Int, Int) -> Position -> Bool
outOfBounds (maxX, maxY) (x, y) = x >= maxX || x < 0 || y >= maxY || y < 0

step :: (Int, Int) -> [Position] -> (Position, Direction) -> Maybe (Position, Direction)
step bounds obs h = let (p, d) = theoreticalNext h in 
    if outOfBounds bounds p then Nothing
    else if any (== p) obs then Just (fst h, turnRight d)
    else Just (p, d)

tracePath :: (Int, Int) -> [Position] -> [(Position, Direction)] -> [(Position, Direction)]
tracePath bounds obs (h:glt) = 
    case step bounds obs h of
        Nothing -> h:glt 
        Just next -> tracePath bounds obs (next:h:glt)

removeDuplicates :: Eq a => [a] -> [a] -> [a]
removeDuplicates acc [] = acc
removeDuplicates acc (h:t) = if any (== h) acc then removeDuplicates acc t else removeDuplicates (h:acc) t

solve :: ([Position], (Position, Direction), (Int, Int)) -> Int
solve (obs, g, bounds) = length $ removeDuplicates [] $ fmap fst $ tracePath bounds obs [g]

isCycle :: (Int, Int) -> [Position] -> [(Position, Direction)] -> Bool
isCycle bounds obs (h:glt) =
    case step bounds obs h of 
        Nothing -> False
        Just (p', d') -> (any (\(p, d) -> p == p' && d `direq` d') (h:glt)) || isCycle bounds obs ((p',d'):h:glt)

everyPosition :: (Int, Int) -> [Position]
everyPosition (boundX, boundY) = concatMap (\i' -> fmap (\ j' -> (i', j')) ly) lx
    where (lx, ly) = ([0..(boundX - 1)], [0..(boundY - 1)])

solvePart2 :: ([Position], (Position, Direction), (Int, Int)) -> Int
solvePart2 (obs, g, bounds) = length 
                            $ filter (\x -> x) 
                            $ fmap (\p -> isCycle bounds (if any (== p) obs then obs else p:obs) [g])  
                            $ everyPosition bounds

testFmapi :: () -> IO ()
testFmapi () = 
    let _ = assert (fmapi (\i x -> i * x) 0 [5, 3, 1] == [0, 3, 2]) in
    let _ = assert (fmapi (\i x -> 999) 14 [] == []) in
    let _ = assert (fmapi (\i x -> i) (-3) [1, 1, 1, 1] == [-3, -2, -1, 0]) in
    print "test fmapi OK"

testFindIndex :: () -> IO ()
testFindIndex () =
    let _ = assert (findIndex (=='a') 0 "baasdjfgha" == [1, 2, 9]) in
    let _ = assert (findIndex (\x -> True) 13 "" == []) in
    let _ = assert (findIndex (\x -> False) 14 [1, 2, 3, 4, 5] == []) in
    let _ = assert (findIndex (> 3) (-4) [1, 2, 3, 4, 5] == [-1, 0]) in
    print "test findIndex OK"

testFindGuard :: () -> IO ()
testFindGuard () =
    let _ = assert (fst (findGuard [".....", "...^.", "....."]) == (1, 3)) in
    print "test findGuard OK"

testTheoreticalNext :: () -> IO ()
testTheoreticalNext () =
    let _ = assert ((fst $ theoreticalNext ((1, 4), Dright)) == (2, 4)) in
    let _ = assert ((fst $ theoreticalNext ((0, 0), Ddown)) == (0, 1)) in
    let _ = assert ((fst $ theoreticalNext ((0, 0), Dleft)) == (-1, 0)) in
    let _ = assert ((fst $ theoreticalNext ((0, 0), Dup)) == (-1, 0)) in
    print "test theoreticalNext OK"

testOutOfBounds :: () -> IO ()
testOutOfBounds () =
    let _ = assert (outOfBounds (50, 20) (-1, 18)) in
    let _ = assert (outOfBounds (0, 0) (0, 0)) in
    let _ = assert (not $ outOfBounds (100, 80) (0, 79)) in
    let _ = assert (outOfBounds (7, 6) (8, 3)) in
    print "test outOfBounds OK"

testStep :: () -> IO ()
testStep () =
    let _ = assert (case step (10, 10) [(2, 1), (1, 1), (-1, 0), (1, 2), (0, 0)] ((0, 1), Ddown) of 
                        Nothing -> False
                        Just ((x, y), d) -> (x, y) == (0, 2)) in
    let _ = assert (case step (10, 10) [(2, 1), (1, 1), (-1, 0), (1, 2), (0, 0)] ((0, 1), Dright) of 
                        Nothing -> False
                        Just ((x, y), d) -> (x, y) == (0, 1)) in
    print "test step OK"

testTracePath :: ([Position], (Position, Direction), (Int, Int)) -> IO ()
testTracePath (obs, g, bounds) =
    print $ fmap fst $ tracePath bounds obs [g]

testRemoveDuplicates :: () -> IO ()
testRemoveDuplicates () =
    let _ = assert (removeDuplicates [] [1, 2, 2, 3, 5, 4, 3, 2, 4, 4, 5, 1, 1, 5, 2, 3] == [4, 5, 3, 2, 1]) in 
    print "test removeDuplicates OK"

testEveryPosition :: () -> IO ()
testEveryPosition () =
    let _ = assert (everyPosition (2, 3) == [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]) in 
    print "test everyPosition OK"

main = do
    input <- readFile "j6/input.txt"
    testFmapi ()
    testFindIndex ()
    testFindGuard ()
    testTheoreticalNext ()
    testOutOfBounds ()
    testStep ()
    testRemoveDuplicates ()
    testEveryPosition ()
    -- testTracePath $ parse $ splitOnChar '\n' input
    print $ solvePart2 $ parse $ splitOnChar '\n' input
