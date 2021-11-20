fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- foldTree

height :: Tree a -> Integer
height Leaf = -1
height (Node _ left _ right) = 1 + max (height left) (height right)

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left val right)
  | height left <= height right = Node (height (Node 0 (insert x left) val right)) (insert x left) val right
  | otherwise                   = Node (height (Node 0 left val (insert x right))) left val (insert x right)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr1 (/=)

-- Sieve of Sundaram

cartProd' :: [a] -> [b] -> [(a, b)]
cartProd' xs ys = [(x,y) | x <- xs, y <- ys]

cartProd :: [Integer] -> [Integer] -> [(Integer, Integer)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys, x <= y]

removedNumber :: (Integer, Integer) -> Integer
removedNumber (i, j) = i + j + 2 * i * j

removedNumbers :: Integer -> [(Integer, Integer)] -> [Integer]
removedNumbers n xs = takeWhile (<= n) $ map removedNumber xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * i + 1 | i <- [1..n], i `notElem` removedNumbers n (cartProd [1..n] [1..n])]
