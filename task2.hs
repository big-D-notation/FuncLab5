data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Eq)

season :: Month -> String
season month
  | month `elem` [December, January, February] = "Winter"
  | month `elem` [March, April, May] = "Spring"
  | month `elem` [June, July, August] = "Summer"
  | otherwise = "Autumn"

daysInMonth :: Int -> Month -> Int
daysInMonth year month
  | month `elem` [April, June, September, November] = 30
  | month == February = if isLeapYear year then 29 else 28
  | otherwise = 31

isLeapYear :: Int -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise = False

main :: IO ()
main = do
  let year = 2023
      months = [January, February, March, April, May, June, July, August, September, October, November, December]
  putStrLn "Seasons:"
  mapM_ (\month -> putStrLn $ show month ++ ": " ++ season month) months
  putStrLn "\nDays in each month:"
  mapM_ (\month -> putStrLn $ show month ++ ": " ++ show (daysInMonth year month) ++ " days") months
