fatD' :: Int -> Int 
fatD' n | n == 0 = 1
        | n == 1 = 1
        | n > 0 = fatD'(n - 2) * n


qc' :: Int -> Int -> Int
qc' _ 0 = error "Divisão indeterminada!"
qc' x y | x < y = 0
        | otherwise = 1 + qc' (x - y) y

r' :: Int -> Int -> Int 
r' _ 0 = error "Divisão indeterminada!"
r' x y | x < y = x
       | otherwise = r' (x - y) y


pot' :: Float -> Float -> Float
pot' x n | n == 0 = 1
         | n > 0 = x * pot' x (n - 1)


nand' :: Bool -> Bool -> Bool
nand' x y = if x == True && y == True then False else True

nand'' :: Bool -> Bool -> Bool
nand'' x y | x == True && y == True = False
           | otherwise = True

nand''' :: Bool -> Bool -> Bool 
nand''' True True = False
nand''' _ _ = True