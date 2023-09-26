palindromo' :: IO()
palindromo' = do putStrLn "Digite uma frase: "
                 str <- getLine
                 if reverse str == str then putStrLn "Eh palindromo!"
                 else putStrLn "Nao eh palindromo!"


produto' :: IO()
produto' = do putStrLn "Digite tres numeros: "
              x1 <- readLn::IO Float
              x2 <- readLn::IO Float
              x3 <- readLn::IO Float
              putStrLn "O produto eh: "
              putStrLn (show(x1 * x2 * x3))


temp' :: IO()
temp' = do putStrLn "Digite uma temperatura em Fahrenheit: "
           f <- readLn::IO Double
           putStrLn "A temperatura em Fahrenheit eh: "
           putStrLn (show(f))
           putStrLn "A temperatura em Celsius eh: "
           putStrLn (show(celsius' f))

celsius' :: Double -> Double
celsius' f = (5/9) * (f - 32)


situacao' :: IO()
situacao' = do putStrLn "Digite a nota de tres alunos: "
               n1 <- readLn::IO Float
               n2 <- readLn::IO Float
               n3 <- readLn::IO Float
               putStr "A media eh: "
               putStrLn (show(fm' n1 n2 n3))
               if (fm' n1 n2 n3) < 3 then putStrLn "Reprovado!" 
               else if (fm' n1 n2 n3) >= 3 && (fm' n1 n2 n3) <= 7 then putStrLn "Exame especial!" 
               else putStrLn "Aprovado!"

fm' :: Float -> Float -> Float -> Float
fm' n1 n2 n3 = (n1 + n2 + n3) / 3


eleitor' :: IO()
eleitor' = do putStrLn "Digite sua idade: "
              idd <- readLn::IO Int 
              putStrLn "Classe Eleitoral: "
              putStrLn "------------------"
              putStr "Idade: "
              putStrLn (show(idd))
              if idd < 16 then putStrLn "Nao eleitor" 
              else if (idd >= 16 && idd < 18) || idd > 65 then putStrLn "Eleitor facultativo"
              else putStrLn "Eleitor obrigatorio"


menu' :: IO()
menu' = do putStrLn "Escolha uma opcao: "
           putStrLn "1 - Salvar uma frase em um arquivo de texto"
           putStrLn "2 - Imprimir o conteudo do arquivo de texto"
           putStrLn "3 - Sair"
           op <- getLine
           case op of 
                "1" -> salvarFrase'
                "2" -> imprimirFrase'
                "3" -> putStrLn "Saindo..."
                _ -> putStrLn "Opcao Invalida!"

salvarFrase' :: IO()
salvarFrase' = do putStrLn "Digite uma frase: "
                  x <- getLine
                  writeFile "frase.txt" x
                  putStrLn "Frase salva!"
                  menu'

imprimirFrase' :: IO()
imprimirFrase' = do x <- readFile "frase.txt"
                    putStrLn "Conteudo do arquivo de texto: "
                    putStrLn x
                    menu'



