toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = res : toDigitsRev ((n - res) `div` 10)
  where res = n `mod` 10

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:[]) = [x]
doubleEveryOtherHelper (x:y:zs) = x : 2 * y : doubleEveryOtherHelper zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherHelper $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0

main :: IO()
main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
  