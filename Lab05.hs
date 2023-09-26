import Data.Char -- Usar a função toUpper e ord

type X = Float
type Y = Float

type Ponto = (X, Y)

ponto1, ponto2 :: Ponto
ponto1 = (7, 5)
ponto2 = (3, 10)

d' :: Ponto -> Ponto -> Float
d' (x1, y1) (x2, y2) = sqrt ((x1 - x2)** 2 + (y1 - y2)** 2)

converte' :: Char -> (Char, Char, Int)
converte' x = (x, toUpper x, ord x) 


equacao' :: (Float, Float, Float) -> (Float, Float)
equacao' (a, b, c) | delta < 0 = error("Nao existe raizes reais!")
                  | delta >= 0 = (x1, x2)
                  where delta = b * b -4*a*c
                        x1 = ((-b + sqrt (delta))/ 2 * a)
                        x2 = ((-b - sqrt (delta))/ 2 * a)


type Nome = String
type Idade = Float
type Sexo = String

type Pessoa = (Nome, Idade, Sexo)

pessoa' :: Float -> Pessoa
pessoa' 1 = ("Arthur", 19, "Masculino")
pessoa' 2 = ("Tannus", 27, "Feminino")

pessoa'' :: Float -> Pessoa
pessoa'' i | i == 1 = ("Arthur", 19, "Masculino")
           | i == 2 = ("Tannus", 27, "Feminino")
                        
idade' :: Pessoa -> Idade
idade' (_, x, _) = x

sumId' :: Float -> Float
sumId' x | x == 1 = idade'(pessoa' 1)
         | x > 1 = idade' (pessoa' x) + sumId' (x - 1)

mediaId' :: Float -> Float
mediaId' x = sumId' x / x