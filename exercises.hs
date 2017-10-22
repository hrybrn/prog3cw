--exercise 1
subtotal :: Num a => [a] -> [a]
subtotal (x:[]) = [x]
subtotal (x:y:xs) = [x] ++ subtotal ([z] ++ xs)
    where
        z = x + y

--exercise 2
--histogram :: Int -> [Int] -> [Int]

--exercise 3
meetsOffer :: String -> Num -> Bool
meetsOffer (x:xs) y =

gradeToPoints :: Char -> Num
gradeToPoints x = 