merge :: [Integer] -> [Integer] -> [Integer]
merge [] [] = []
merge (h:t) [] = h:t
merge [] (h:t) = h:t
merge (h1:t1) (h2:t2)
  | h1 < h2 = h1 : merge t1 (h2:t2)
  | otherwise = h2 : merge (h1:t1) t2
  
split :: [Integer] -> ([Integer], [Integer])
split [] = ([], [])
split (h:t) = let (g, d) = split t in ((h:d), g)

sort :: [Integer] -> [Integer]
sort [] = []
sort [x] = [x]
sort l = let (g, d) = split l in merge (sort g) (sort d)

main = print(sort [4, 6, 32, 7, 3, 5, 8])