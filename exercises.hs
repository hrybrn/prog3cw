import Numeric
import Data.Char
import Data.List

--exercise 1
subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:[]) = [x]
subtotal (x:y:xs) = [x] ++ subtotal ([x + y] ++ xs)

--exercise 2
histogram :: Int -> [Int] -> [Int]
histogram n [] = []
histogram n (x:xs)
    | or [(xs /= []),(n < x)] = (howFar n (x:xs)) : (histogram n (remove n (x:xs)))
    | n == x = [0]
    | n > x = []

howFar :: Int -> [Int] -> Int
howFar n (x:xs)
    | n < x = 0
    | n == x = 1
    | otherwise = 1 + howFar (n - x) xs

remove :: Int -> [Int] -> [Int]
remove n (x:xs)
    | n < x = (x - n):xs
    | n == x = xs
    | otherwise = remove (n - x) xs

--exercise 3
meetsOffer :: Ord a => Num a => String -> a -> Bool
meetsOffer [] y = 0 >= y
meetsOffer (x:[]) y = gradeToPoints x >= y
meetsOffer (x:xs) y = meetsOffer xs (y - gradeToPoints x)

gradeToPoints :: Num a => Char -> a
gradeToPoints x
    |   x == 'A' = 40
    |   x == 'B' = 32
    |   x == 'C' = 24
    |   x == 'D' = 16
    |   x == 'E' = 8
    |   otherwise = 8


--exercise 4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving Show
sortType :: Ord a => [a] -> TypeOfSort
sortType xs
    | ascending xs = Ascending
    | ascending (reverse xs) = Descending
    | constant xs = Constant
    | nonDescending xs = NonDescending
    | nonDescending (reverse xs) = NonAscending
    | otherwise = NotSorted

nonDescending :: Ord a => [a] -> Bool
nonDescending [] = True
nonDescending (x:[]) = True
nonDescending (x:y:xs) = y >= x && nonDescending (y:xs)

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending (x:[]) = True
ascending (x:y:xs) = y > x && ascending (y:xs)

constant :: Ord a => [a] -> Bool
constant [] = True
constant (x:[]) = True
constant (x:y:xs) = x == y && constant (y:xs)

--exercise 5
rpcalc :: String -> [Int] -> Int
rpcalc [] [] = 0
rpcalc [] (y:[]) = y
rpcalc (x:xs) (y:[]) = rpcalc xs [digitToInt x, y] 
rpcalc (x:xs) [] = rpcalc xs [digitToInt x]
rpcalc (x:xs) (y:z:ys)
    | x == '+' = rpcalc xs ((z + y):ys) 
    | x == '-' = rpcalc xs ((z - y):ys) 
    | x == '/' = rpcalc xs ((z `div` y):ys) 
    | x == '*' = rpcalc xs ((z * y):ys) 
    | otherwise = rpcalc xs ((digitToInt x):y:z:ys)

--exercise 6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs
    | length xs <= k = xs
    | otherwise = take k sorted
        where sorted = mergesort p xs

mergesort :: (Floating a, Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
mergesort p xs 
    | (length xs) > 1 = mergesortMerge p (mergesort p ls) (mergesort p rs)
    | otherwise = xs
    where (ls, rs) = mergesortSplit xs        

mergesortSplit :: [(a, a)] -> ([(a, a)], [(a, a)])
mergesortSplit xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

mergesortMerge :: (Floating a, Ord a) => (a, a) -> [(a ,a)] -> [(a ,a)] -> [(a ,a)]
mergesortMerge p [] xs = xs
mergesortMerge p xs [] = xs
mergesortMerge p (x:xs) (y:ys)
    | (distance p x < distance p y) = x:mergesortMerge p xs (y:ys)
    | otherwise = y:mergesortMerge p (x:xs) ys    

distance :: (Floating a, Ord a) => (a,a) -> (a,a) -> a
distance x y = sqrt ((fst x - fst y) ^ 2 + (snd x - snd y) ^ 2)

--exercise 7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

balanced :: SearchTree -> Bool
balanced (Leaf i) = True
balanced (Node left i right) = balanced left && balanced right && (abs(nodes left - nodes right) < 2) 

nodes :: SearchTree -> Int
nodes (Leaf i) = 1
nodes (Node left i right) = nodes left + nodes right

unbalancedTree :: SearchTree
unbalancedTree =
    Node
        (Leaf 4)
        5
        (Node
            (Node
                (Leaf 2)
                5
                (Leaf 8)
            )
            5
            (Leaf 8)
        )

balancedTree :: SearchTree
balancedTree =
    Node
        (Node
            (Leaf 3)
            4
            (Leaf 9)
        )

        5

        (Node
            (Leaf 2)
            5
            (Leaf 8)
        )

--exercise 8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = [getRoot d x | x <- [0..]]

getRoot :: Double -> Double -> Double
getRoot d x
    | x == 0 = 1
    | otherwise = (y + (d/y)) / 2 
        where y = getRoot d (x - 1)

newtonRoot :: Double -> Double -> Double 
newtonRoot d epsilon = head [getRoot d x | x <- [1..], abs((getRoot d x) - getRoot d (x - 1)) - abs(epsilon) < 0]
    
--exercise 9
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator x y z
    | x == 0 = z + 1
    | x == 1 && z == 0 = y
    | x == 2 && z == 0 = 0
    | x >= 3 && z == 0 = 1
    | otherwise = hyperOperator (x - 1) y (hyperOperator x y (z - 1))

--exercise 10
encode :: String -> [Int]
encode [] = []
encode (x:xs) = digits ++ encode xs 
    where   digits = addParity 0 (charToBinary 128 (fromEnum x))

charToBinary :: Int -> Int -> [Int]
charToBinary n x 
    | n == 0 = []
    | n <= x = 1 : charToBinary (n `div` 2) (x - n)
    | otherwise = 0 : charToBinary (n `div` 2) (x)

addParity :: Int -> [Int] -> [Int]
addParity n [] = [n `mod` 2]
addParity n (x:xs) = x : addParity (n + x) xs

--exercise 11
decode :: [Int] -> String
decode [] = ""
decode xs
    | checkParity 0 xs = ""
    | length xs `mod` 9 /= 0 = ""
    | otherwise = binaryToChar 0 (take 8 xs) : decode (drop 9 xs)

checkParity :: Int -> [Int] -> Bool
checkParity n [] = n `mod` 2 /= 0
checkParity n (x:xs) = checkParity (n + x) xs

binaryToChar :: Int -> [Int] -> Char
binaryToChar n [] = toEnum n
binaryToChar n (x:xs) = binaryToChar (n+i) xs
    where i = x * (2 ^ (length xs))

--exercise 12
--exercise 13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence xs = [getSuccessor n xs | n <- [0..], snd (getSuccessor n xs) /= [0] && fst (getSuccessor n xs) /= 0]

getSuccessor :: Int -> (Int, [Int]) -> (Int, [Int])
getSuccessor n x 
    | n == 0 = x
    | otherwise = getSuccessor (n - 1) (successor x)

successor :: (Int, [Int]) -> (Int, [Int])
successor (n, [0]) = (0 , [0])
successor (n, x:xs)
    | x > 0 = (n + 1, trimEnd ((x - 1):xs))
    | otherwise = (n + 1, final)
        where   value = head (filter (0 <) xs)
                index = position value (x:xs)
                res = replace (index - 1) n (replace index (value - 1) (x:xs))
                final = trimEnd res

trimStart :: [Int] -> [Int]
trimStart (x:xs)
    | x == 0 = trimStart xs
    | otherwise = (x:xs) 
                   
trimEnd :: [Int] -> [Int]
trimEnd xs = reverse (trimStart (reverse xs))

replace :: Int -> a -> [a] -> [a]
replace n new (x:xs)
    | n == 0 = new:xs
    | otherwise = x:replace (n-1) new xs

position :: Int -> [Int] -> Int
position search [] = -1
position search (x:xs)
    | search == x = 0
    | otherwise = 1 + position search xs

--exercise 14
--type Subst = Assoc Char Bool
--type Assoc k v = [(k,v)]
--
--data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
--isSat :: Prop -> [Subst]
--isSat input = findSolutions input (vars input) []
--
--findSolutions :: Prop -> [Subst] -> [Subst] -> [Subst]
--findSolutions input [] ys = ys
--findSolutions input (x:xs) ys
--    | eval x input = findSolutions input xs (x:ys)
--    | otherwise = findSolutions input xs ys
--
--vars :: Prop -> [Char]
--vars (Const _) = []
--vars (Var x) = [x]
--vars (Not p) = vars p
--vars (And p q) = vars p ++ vars q
--vars (Imply p q) = vars p ++ vars q
--
--eval :: Subst -> Prop -> Bool
--eval s (Const b) = b
--eval s (Var c) = find c s
--eval s (Not p) = not $ eval s p
--eval s (And p q) = eval s p && eval s q
--eval s (Imply p q) = eval s p <= eval s q

--exercise 15
isCantorPair :: Int -> Bool
isCantorPair z = addTuple (invertPair (fst invert)) == snd invert
    where invert = invertPair z

addTuple :: (Int, Int) -> Int
addTuple (x,y) = x + y

invertPair :: Int -> (Int, Int)
invertPair z = (x, y)
    where   w = floor (((sqrt (8 * (fromIntegral z) + 1)) - 1) / 2)
            t = (w * (w + 1)) `div` 2
            y = z - t
            x = w - y

pair :: Int -> Int -> Int
pair x y = y + (xysum) * (xysum + 1) `div` 2
    where xysum = x + y