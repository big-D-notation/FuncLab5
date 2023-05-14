data Tree a = Node a (Tree a) (Tree a) | Nil

minMax :: Ord a => Tree a -> Maybe (a, a)
minMax Nil = Nothing
minMax (Node x left right) =
  let (minL, maxL) = case minMax left of
        Nothing -> (x, x)
        Just (minVal, maxVal) -> (minVal, maxVal)
      (minR, maxR) = case minMax right of
        Nothing -> (x, x)
        Just (minVal, maxVal) -> (minVal, maxVal)
      minValue = min x (min minL minR)
      maxValue = max x (max maxL maxR)
   in Just (minValue, maxValue)

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node x left right) = flatten left ++ [x] ++ flatten right

sInsert :: Ord a => a -> Tree a -> Tree a
sInsert value Nil = Node value Nil Nil
sInsert value (Node x left right)
  | value <= x = Node x (sInsert value left) right
  | otherwise = Node x left (sInsert value right)

tsort :: Ord a => [a] -> [a]
tsort = flatten . foldr sInsert Nil

telem :: Eq a => a -> Tree a -> Bool
telem _ Nil = False
telem value (Node x left right) = value == x || telem value left || telem value right

showT' :: Show a => Int -> Tree a -> String
showT' _ Nil = ""
showT' indent (Node x left right) =
  spaces indent
    ++ show x
    ++ "\n"
    ++ showT' (indent + 4) left
    ++ showT' (indent + 4) right
  where
    spaces n = replicate n ' '

bal :: [a] -> Tree a
bal [] = Nil
bal xs =
  let (left, mid, right) = splitList xs
   in Node mid (bal left) (bal right)
  where
    splitList [] = ([], error "Empty List", [])
    splitList [x] = ([], x, [])
    splitList xs =
      let (ys, zs) = splitAt (length xs `div` 2) xs
       in (ys, head zs, tail zs)

main :: IO ()
main = do
  let tree = Node 3 (Node 2 (Node 1 Nil Nil) Nil) (Node 5 (Node 4 Nil Nil) Nil)
      list = [5, 2, 3, 4, 1]

  putStrLn "Minimum and Maximum Values:"
  print (minMax tree)
  putStrLn ""

  putStrLn "Flattened Tree:"
  print (flatten tree)
  putStrLn ""

  putStrLn "Sorted List:"
  print (tsort list)
  putStrLn ""

  putStrLn "Check Element:"
  print (telem 3 tree)
  print (telem 6 tree)
  putStrLn ""

  putStrLn "Tree Visualization:"
  putStr (showT' 0 tree)
  putStrLn ""

  putStrLn "Balanced Tree:"
  putStr (showT' 0 (bal list))
  putStrLn ""

  putStrLn "Folded Tree:"
  putStr (showT' 0 (foldr sInsert Nil list))
