ordena' :: Int -> Int -> (Int, Int)
ordena' x y = if x > y then (y, x) else (x, y) 


ordena'' :: Int -> Int -> (Int, Int)
ordena'' x y | x > y = (y, x)
             | otherwise = (x, y)


meses' :: Int -> String
meses' x | x == 1 = "Janeiro"
         | x == 2 = "Fevereiro"
         | x == 3 = "Marco"
         | x == 4 = "Abril"
         | x == 5 = "Maio"
         | x == 6 = "Junho"
         | x == 7 = "Julho"
         | x == 8 = "Agosto"
         | x == 9 = "Setembro"
         | x == 10 = "Outubro"
         | x == 11 = "Novembro"
         | x == 12 = "Dezembro"
         | otherwise = error("Invalido!")


meses'' :: Int -> String
meses'' x = case x of
            1 -> "Janeiro"
            2 -> "Fevereiro"
            3 -> "Marco"
            4 -> "Abril"
            5 -> "Maio"
            6 -> "Junho"
            7 -> "Julho"
            8 -> "Agosto"
            9 -> "Setembro"
            10 -> "Outubro"
            11 -> "Novembro"
            12 -> "Dezembro"
            _ -> error("Invalido")

-- O if then else não é interessante neste caso, pois temos muitas condições, logo elese torna ineficiente --


triangulo' :: Float -> Float -> Float -> String
triangulo' x y z | not existe = "Nao eh triangulo!"
                 | x == y && y == z && z == x = "Equilatero"
                 | (x == y) || (z == y) || (x == z) = "Isoceles"
                 | x /= y && x /= z && z /= y = "Escaleno"
                 where existe = x > 0 && y > 0 && z > 0 && x + y > z && x + z > y && z + y > x


maior' :: Float->Float->Float
maior' x y = max x y

menor' :: Float->Float->Float
menor' x y = min x y

diferenca' :: Float->Float->Float
diferenca' x y = ma - me
               where ma = maior' x y 
                     me = menor' x y

-- A função "diferenca'" é declarada com dois valores x e y, após isso temos a redução do "ma" pelo "me" dentro do escopo de where onde declaramos "ma" e "me" chamando as funções "maior'" e "menor'" evitando números negativos


areaC' :: Float->Float
areaC' d = pi * (r**2)
        where r = d/2


raizes' :: Float->Float->Float->(Float, Float)
raizes' a b c = (bhaskara', bhaskara'')
                 where delta = (b**2) -4*a*c
                       bhaskara' = (-b + (sqrt delta)) / (2*a)
                       bhaskara'' = (-b - (sqrt delta)) / (2*a)


numRaizes' :: Float->Float->Float->Int
numRaizes' a b c | delta > 0 = 2
                 | delta == 0 = 1
                 | delta < 0 = 0
                 where delta = (b**2) -4*a*c


menu' :: Int->Float->Float->Float
menu' m x y = case x of 
              1 -> x + y
              2 -> diferenca' x y
              3 -> (x * y)
              4 -> divisao' x y
              _ -> error("Opcao Invalida!")


divisao' :: Float->Float->Float
divisao' x y = if y < 0 then -1 else x / y 