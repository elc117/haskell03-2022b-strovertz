multN :: Int -> [Int] -> [Int]
multN n lst = [x * n | x <- lst]

multN' :: Int -> [Int] -> [Int]
multN' n lst = map (\x -> x * n) lst

calcExpr :: [Float] -> [Float]
calcExpr lst = [x^2/2 | x <- lst, (x^2/2) > 10]

applyExpr :: [Int] -> [Int]
applyExpr lst = [3*x + 2 | x <- lst]

applyExpr' :: [Int] -> [Int]
applyExpr' lst = map (\x -> 3*x + 2) lst

addSuffix :: String -> [String] -> [String]
addSuffix suffix lst = [x ++ suffix | x <- lst]

selectgt5 :: [Int] -> [Int]
selectgt5 lst = [x | x <- lst, x > 5]

sumOdds :: [Int] -> Int
sumOdds lst = sum [x | x <- lst, odd x]

add10toall :: [Int] -> [Int]
add10toall lst = [x + 10 | x <- lst]

sumOdds' :: [Int] -> Int
sumOdds' lst = foldl (\acc x -> if odd x then acc + x else acc) 0 lst

selectExpr :: [Int] -> [Int]
selectExpr lst = [x | x <- lst, even x , x >= 20 , x <= 50]

countShorts :: [String] -> Int
countShorts lst = length [x | x <- lst, length x < 5]

trSpaces :: String -> String
trSpaces str = [if x == ' ' then '-' else x | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd lst = [snd x | x <- lst]

dotProd :: [Int] -> [Int] -> Int
dotProd lst1 lst2 = sum [x*y | (x,y) <- zip lst1 lst2]