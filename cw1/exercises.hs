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
rpcalc :: String -> Int
rpcalc x = rpcalcMain x []

rpcalcMain :: String -> [Int] -> Int
rpcalcMain [] [] = 0
rpcalcMain [] (y:[]) = y
rpcalcMain (x:xs) (y:[]) = rpcalcMain xs [digitToInt x, y] 
rpcalcMain (x:xs) [] = rpcalcMain xs [digitToInt x]
rpcalcMain (x:xs) (y:z:ys)
    | x == '+' = rpcalcMain xs ((z + y):ys) 
    | x == '-' = rpcalcMain xs ((z - y):ys) 
    | x == '/' = rpcalcMain xs ((z `div` y):ys) 
    | x == '*' = rpcalcMain xs ((z * y):ys) 
    | otherwise = rpcalcMain xs ((digitToInt x):y:z:ys)

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

balancedSize :: SearchTree -> Bool
balancedSize (Leaf i) = True
balancedSize (Node left i right) = balancedSize left && balancedSize right && (abs(nodes left - nodes right) < 2)

balanced :: SearchTree -> Bool
balanced x = balancedValues x && balancedSize x 

balancedValues :: SearchTree -> Bool
balancedValues (Leaf i) = True
balancedValues (Node left i right) = highest left <= i && i <= lowest right && balancedValues left && balancedValues right

highest :: SearchTree -> Int
highest (Leaf i) = i
highest (Node left i right) = last (sort [highest left, i, highest right])

lowest :: SearchTree -> Int
lowest (Leaf i) = i
lowest (Node left i right) = head (sort [lowest left, i, lowest right])

nodes :: SearchTree -> Int
nodes (Leaf i) = 1
nodes (Node left i right) = nodes left + nodes right

--exercise 8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = [getRoot d x | x <- [0..]]

getRoot :: Double -> Double -> Double
getRoot d x
    | x == 0 = 1
    | otherwise = (y + (d/y)) / 2 
        where y = getRoot d (x - 1)

newtonRoot :: Double -> Double -> Double 
newtonRoot d epsilon = head [getRoot d x | x <- [1..], abs((getRoot d x) - getRoot d (x - 1)) <= abs(epsilon)]
    
--exercise 9
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator x y z
    | x == 0 = z + 1
    | x == 1 = y + z
    | x == 2 = y * z
    | x == 3 = y ^ z
    | otherwise = knuth (x - 2) y z

knuth :: Int -> Int -> Int -> Int
knuth x y z
    | y == 0 = 1
    | x == 1 = y ^ z
    | otherwise = knuth (x - 1) y (knuth (x - 1) y (z - 1))

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
makeChange :: Int -> [Int] -> [Int]
makeChange m denoms = solution
    where
        denomLength = length denoms
        empty = genEmpty denomLength
        solution = iterateChange m denoms empty

genEmpty :: Int -> [Int]
genEmpty n
    | n == 0 = []
    | otherwise = 0 : genEmpty (n - 1)

iterateChange :: Int -> [Int] -> [Int] -> [Int]
iterateChange target denoms input
    | changeTotal denoms input == target = input
    | otherwise = head [iterateChange target denoms x | x <- next, changeTotal denoms input == target]
        where next = genCombinations 0 input

genCombinations :: Int -> [Int] -> [[Int]]
genCombinations n xs
    | n < length xs = [ if x == n then (xs !! n) + 1 else xs !! n | x <- [0..len]] : genCombinations (n + 1) xs
    | otherwise = []
        where len = length xs - 1

changeTotal :: [Int] -> [Int] -> Int
changeTotal _ [] = 0
changeTotal [] _ = 0
changeTotal (x:xs) (y:ys) = x * y + changeTotal xs ys

--exercise 13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (a,b)
    | b == [] = [(a,b)]
    | otherwise = (a,b) : goodsteinSequence (successor (a,b))

successor :: (Int, [Int]) -> (Int, [Int])
successor (n, x:xs)
    | x > 0 = (n + 1, trimEnd ((x - 1):xs))
    | otherwise = (n + 1, final)
        where   value = head (filter (0 <) xs)
                index = position value (x:xs)
                res = replace (index - 1) n (replace index (value - 1) (x:xs))
                final = trimEnd res

trimStart :: [Int] -> [Int]
trimStart [] = []
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
type Assoc k v = [(k,v)]

findo :: Eq k => k -> Assoc k v -> v
findo k t = head [v | (k',v) <- t, k == k']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = findo x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
             where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

isSat :: Prop -> [Subst]
isSat p = [x | x <- possible, eval x p]
        where possible = substs p

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