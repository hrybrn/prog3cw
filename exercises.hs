--exercise 1
subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:[]) = [x]
subtotal (x:y:xs) = [x] ++ subtotal ([x + y] ++ xs)

--exercise 2
--histogram :: Int -> [Int] -> [Int]

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
--data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted
--sortType :: Ord a => [a] -> TypeOfSort
--sortType (x:y:xs) = 

--exercise 5
--rpcalc :: Num a => String -> [a] -> [a]

--exercise 8
--newtonRootSequence :: Double -> [Double]
--newtonRootSequence x = iterate g x
--    where
--        d = head (newtonRootSequence x)
--        f = toInteger (x + d/x)
--        g = fromInteger (f div 2)

--exercise 9
hyperOperator :: Ord a => a -> a -> a -> a
hyperOperator x y z
    | x == 0 = z + 1
    | x == 1 && z == 0 = y
    | x == 2 && z == 0 = 0
    | x >= 3 && z == 0 = 1
    | otherwise = hyperOperator (x - 1) y (hyperOperator x y (z - 1))
