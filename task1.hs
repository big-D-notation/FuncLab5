search :: Eq a => a -> b -> [(a, b)] -> b
search k fail [] = fail
search k fail ((a, b) : xs)
  | k == a = b
  | otherwise = search k fail xs

main :: IO ()
main = do
  let pairs = [("apple", 5), ("banana", 7), ("orange", 3)]
      result1 = search "banana" 0 pairs
      result2 = search "grape" 0 pairs
  putStrLn ("Value associated with key 'banana': " ++ show result1)
  putStrLn ("Value associated with key 'grape': " ++ show result2)
