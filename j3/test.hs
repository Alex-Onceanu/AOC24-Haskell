import Text.Regex.Posix

regex = "do\\(\\)|don't\\(\\)"

main :: IO ()
main = do
  let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  let output = getAllTextMatches (input =~ regex :: AllTextMatches [] String)
  print $ fmap (== "do()") output