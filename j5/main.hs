import Data.Graph

initList :: Int -> a -> [a]
initList 0 x = []
initList n x = x:(initList (n - 1) x)

myTail :: [a] -> [a]
myTail (_:t) = t
myTail _ = error "tail of []"

splitOnChar :: Char -> String -> [String]
splitOnChar sep s = let (h, t) = break (== sep) s in 
    case t of 
        "" -> [h]
        _ : sNext -> h : (splitOnChar sep sNext)

firstTwo :: [a] -> (a, a)
firstTwo (h1:h2:_) = (h1, h2)
firstTwo _ = error "firstTwo of []"

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f1 f2 = \x -> f1 x && f2 x

parseInstr :: [String] -> [[Int]]
parseInstr = fmap ((fmap read) . (splitOnChar ','))

parseRules :: [String] -> [(Int, Int)]
parseRules = fmap (firstTwo . (fmap read) . (splitOnChar '|'))

parse :: [String] -> ([(Int, Int)], [[Int]])
parse sl = (parseRules rules, parseInstr $ myTail instr)
    where (rules, instr) = break (=="") sl

middle :: [Int] -> Int
middle l = l !! ((length l) `div` 2)

doneBefore :: Int -> Int -> [Int] -> Bool
doneBefore first second [] = error "didnt find first nor second in doneBefore"
doneBefore first second (h:t) = h == first || (if h == second then False else doneBefore first second t)

illegalRule :: [Int] -> Int -> (Int, Int) -> Bool
illegalRule done task (page1, page2) = task == page2 && doneBefore page2 page1 done

isTaskValid :: [Int] -> Int -> [(Int, Int)] -> Bool
isTaskValid done task = all (not . (illegalRule done task))

useful :: (Int, Int) -> [Int] -> Bool
useful (page1, page2) = any (== page1) &&& any (== page2)

isInstrValid :: [(Int, Int)] -> [Int] -> Bool
isInstrValid rules instr = all (flip (isTaskValid instr) $ filter (flip useful $ instr) rules) instr

solve :: ([(Int, Int)], [[Int]]) -> Int
solve (rules, instrs) = foldl1 (+) $ fmap middle $ filter (isInstrValid rules) instrs

graphOfRules :: [(Int, Int)] -> [Int] -> Graph
graphOfRules rules instr = buildG (0, 99) (filter (flip useful $ instr) rules)

correct :: [(Int, Int)] -> [Int] -> [Vertex]
correct rules instr = filter (\v -> any (== v) instr) $ topSort $ graphOfRules rules instr

solvePart2 :: ([(Int, Int)], [[Int]]) -> Int
solvePart2 (rules, instrs) = foldl1 (+) $ fmap (middle . (correct rules)) $ filter (not . (isInstrValid rules)) instrs

main = do
    input <- readFile "j5/input.txt"
    -- print $ solve $ parse $ splitOnChar '\n' input
    print $ solvePart2 $ parse $ splitOnChar '\n' input
