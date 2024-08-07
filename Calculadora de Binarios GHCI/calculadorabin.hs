{-|
Aluno: Malcon Rezende Rodrigues
Matrícula: 12211BCC034
Primeiro Trabalho Programação Funcional
|-}

-- | BIBLIOTECA DE FUNCOES IO
import System.IO (stdout, stdin, hFlush, hSetBuffering, BufferMode(NoBuffering), BufferMode(LineBuffering))

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM BINARIO PARA DECIMAL
menuBintoDec :: IO ()
menuBintoDec = do 
          
       putStr "\n\nDigite um binario: "
       string <- getLine
       imprimeBintoDec string
       saidaBintoDec
       
       
imprimeBintoDec :: String -> IO ()
imprimeBintoDec string = do

-- | CHAMA A FUNCAO CONBINTODEC (PASSANDO A STRING INVERTIDA E O VALOR 1) GUARDANDO SEU VALOR EM DECIMAL
       let decimal = conbintoDec (reverse string) 1

       putStr "\n"
       putStr string
       putStr "_(2) = "
       putStr (show decimal)
       putStr "_(10)"
           
           
saidaBintoDec :: IO ()
saidaBintoDec = do

       putStr "\n\nDeseja converter outro binário (s/n)? "

       x <- getChar

       case x of
           -- | CHAMA A FUNCAO NOVAMENTE
           's' -> menuBintoDec 
           -- | CHAMA O MENU
           'n' -> return ()
           _ -> do putStr "\n\nOpcao Invalida"
                   saidaBintoDec


-- | A FUNCAO DE CONVERSAO RECEBE UMA LISTA DE CHAR (BINARIO) E UM INTEIRO, E DEVOLVE UM INTEIRO
conbintoDec :: [Char] -> Int -> Int 

conbintoDec [] _ = 0 
{-|
    SE A CABEÇA DA LISTA FOR 0, A FUNCAO CHAMA A SI MESMA PASSANDO A CAUDA E O VALOR DO EXPOENTE MULTIPLICADO POR DOIS
    SE FOR 1, A FUNCAO SOMA O VALOR DAQUELA CASA A CHAMADA RECURSIVA, TAMBÉM PASSANDO A CAUDA E O VALOR DO EXPOENTE MULTIPLICADO POR DOIS
    SE NÃO FOR NEM 1 NEM 0, O VALOR PASSADO PARA X É UM BINARIO INVALIDO
|-}

conbintoDec (x:xs) y | (x == '0') = conbintoDec xs (y * 2) 
                     | (x == '1') = y + conbintoDec xs (y * 2) 
                     | otherwise = error "Binário inválido"

{-| EXEMPLO: 1 0 0 0 = 8 + 0 + 0 + 0 = 8
             8 4 2 1
|-}


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM HEXADECIMAL PARA DECIMAL
menuHectoDec :: IO ()
menuHectoDec = do

       putStr "\n\nDigite um hexadecimal: "
       string <- getLine
       imprimeHectoDec string
       saidaHectoDec
       
imprimeHectoDec :: String -> IO ()
imprimeHectoDec string = do

-- | CHAMA A FUNCAO CONHECTODEC (PASSANDO A STRING INVERTIDA E O VALOR 0) GUARDANDO SEU VALOR EM DECIMAL
       let decimal = conhectoDec (reverse string) 0

       putStr "\n"
       putStr string 
       putStr "_(16) = "
       putStr (show decimal)
       putStr "_(10)"
       
saidaHectoDec :: IO ()
saidaHectoDec = do

       putStr "\n\nDeseja converter outro binário (s/n)? "
       
       x <- getChar
       
       case x of
           's' -> menuHectoDec
           'n' -> return ()
           _ -> do putStr "\n\nOpcao Invalida"
                   saidaHectoDec

-- | A FUNCAO DE CONVERSAO RECEBE UMA LISTA DE CHAR (HEXADECIMAL) E UM INTEIRO, E DEVOLVE UM INTEIRO
conhectoDec :: [Char] -> Int -> Int 

-- | (CASO BASE) QUANDO A LISTA ESTIVER VAZIA, INDEPENDENTE DO VALOR DO INTEIRO, VOLTA O VALOR 0
conhectoDec [] _ = 0

{-| 
    TENDO A LISTA PELO MENOS UM CARACTER, VERIFICA SE ESSE VALOR É ALGUMA DAS LETRAS DO CÓDIGO HEXA
    SE FOR ALGUMA DAS LETRAS, MULTIPLICA O VALOR CORRESPONDENTE A 16 ELEVADO AO VALOR DA CASA, E SOMA À CHAMADA RECURSIVA (PASSANDO A CAUDA E ANDANDO UMA CASA)
    SE NAO FOR NENHUMA DAS LETRAS, MULTIPLICA O VALOR CONVERTIDO A 16 ELEVADO AO VALOR DA CASA, E SOMA À CHAMADA RECURSIVA (PASSANDO A CAUDA E ANDANDO UMA CASA)
|-}

conhectoDec (x:xs) y | (x == 'a') || (x == 'A') = (10 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x == 'b') || (x == 'B') = (11 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x == 'c') || (x == 'C') = (12 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x == 'd') || (x == 'D') = (13 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x == 'e') || (x == 'E') = (14 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x == 'f') || (x == 'F') = (15 * (16 ^ y)) + conhectoDec xs (y + 1)
                     | (x >= '0') && (x <= '9') = ((fromEnum x - fromEnum '0') * (16 ^ y)) + conhectoDec xs (y + 1)
                     | otherwise = error "Hexadecimal invalido"

{-| EXEMPLO : 1  A  8 (hexadecimal) = (1 * 16^2) + (10 * 16^1) + (8 * 16^0) = 424
              2  1  0 (casas)
|-}


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM DECIMAL PARA BINARIO
menuDectoBin :: IO ()
menuDectoBin = do

        putStr"\n\nDigite um natural: "
        string <- getLine
        imprimeDectoBin string
        saidaDectoBin
        
imprimeDectoBin :: String -> IO ()
imprimeDectoBin string = do

-- | CHAMA A FUNCAO CONDECTOBIN (PASSANDO A STRING (DECIMAL) CONVERTIDA PARA INTEIRO) GUARDANDO SEU VALOR EM BINARIO
        let binario = condectoBin (read string :: Int) 

        putStr "\n"
        putStr string 
        putStr "_(10) = "
        putStr binario 
        putStr "_(2)"
        
saidaDectoBin :: IO ()
saidaDectoBin = do

        putStr "\n\nDeseja converter outro decimal (s/n)? "

        x <- getChar

        case x of
            's' -> menuDectoBin
            'n' -> return ()
            _ -> do putStr "\n\nOpcao Invalida"
                    saidaDectoBin

-- | A FUNCAO DE CONVERSAO RECEBE UM INTEIRO E DEVOLVE UMA LISTA DE CARACTERES (STRING)
condectoBin :: Int -> [Char]

-- | CASO BASE 1: SE O INTEIRO FOR IGUAL A 0, ADICIONA O CARACTER 0 NA CABECA DA LISTA
condectoBin 0 = ['0']

-- | CASO BASE 2: SE O INTEIRO FOR IGUAL A 1, ADICIONA O CARACTER 1 NA CABECA DA LISTA
condectoBin 1 = ['1'] 

{-| 
   SE O RESTO DA DIVISAO FOR IGUAL A 0, CHAMA A FUNCAO RECURSIVAMENTE PASSANDO A DIVISAO INTEIRA DE X POR 2 E ADICIONA O CARACTER 0 NA LISTA
   JA SE O RESTO DA DIVISAO FOR IGUAL A 1, CHAMA A FUNCAO RECURSIVAMENTE PASSANDO A DIVISAO INTEIRA DE X POR 2 E ADICIONA O CARACTER 1 NA LISTA
|-}

condectoBin x | (mod x 2 == 0) = condectoBin (x `div` 2) ++ ['0'] 
              | (mod x 2 == 1) = condectoBin (x `div` 2) ++ ['1'] -- 

{-| EXEMPLO : 10|2    ->   5|2    ->   2|2    ->   1|2      OS RESTOS: 1 0 1 0 = 10
              0 5         1 2         0 1         1 0
|-}


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM DECIMAL PARA HEXADECIMAL
menuDectoHec :: IO ()
menuDectoHec = do

        putStr "\n\nDigite um natural: "
        string <- getLine
        imprimeDectoHec string
        saidaDectoHec
        
imprimeDectoHec :: String -> IO ()
imprimeDectoHec string = do

-- | CHAMA A FUNCAO CONDECTOHEC (PASSANDO A STRING CONVERTIDA PARA INTEIRO) GUARDANDO SEU VALOR EM HEXADECIMAL
        let hexadecimal = condectoHec (read string :: Int)

        putStr "\n"
        putStr string
        putStr "_(10) = "
        putStr hexadecimal
        putStr "_(16)"
        
saidaDectoHec :: IO ()
saidaDectoHec = do

        putStr "\n\nDeseja converter outro decimal (s/n)? "

        x <- getChar

        case x of
            's' -> menuDectoHec
            'n' -> return ()
            _ -> do putStr "\n\nOpcao Invalida"
                    saidaDectoHec

-- | A FUNCAO DE CONVERSAO RECEBE UM INTEIRO E DEVOLVE UMA LISTA DE CARACTERES (STRING)
condectoHec :: Int -> [Char]

-- | SE O VALOR DO INTEIRO FOR DE 1 A 9, ADICIONA O VALOR CORRESPONDENTE DA TABELA NA LISTA

condectoHec 0 = ['0']
condectoHec 1 = ['1']
condectoHec 2 = ['2']
condectoHec 3 = ['3']
condectoHec 4 = ['4']
condectoHec 5 = ['5']
condectoHec 6 = ['6']
condectoHec 7 = ['7']
condectoHec 8 = ['8']
condectoHec 9 = ['9']

condectoHec 10 = ['A']
condectoHec 11 = ['B']
condectoHec 12 = ['C']
condectoHec 13 = ['D']
condectoHec 14 = ['E']
condectoHec 15 = ['F']

{-| 
   CASO O VALOR DO INTEIRO FOR MAIOR QUE 15, VERIFICA-SE O RESTO DA DIVISAO DO VALOR DO INTEIRO POR 16
   SE O VALOR DO RESTO FOR CORRESPONDENTE A ALGUMA LETRA DA TABELA HEXADECIMAL, CHAMA-SE A FUNCAO RECURSIVAMENTE PASSANDO A DIVISAO INTEIRA E ADICIONANDO A LETRA CORRESPONDENTE NA LISTA
   JA SE O VALOR DO RESTO FOR MAIOR QUE 15, CHAMA-SE A FUNCAO RECURSIVAMENTE PASSANDO A DIVISAO INTEIRA E ADICIONANDO O VALOR DO RESTO (CONVERTIDO PARA STRING) NA LISTA
|-}

condectoHec x | (mod x 16 == 10) = condectoHec (x `div` 16) ++ ['A']
              | (mod x 16 == 11) = condectoHec (x `div` 16) ++ ['B']
              | (mod x 16 == 12) = condectoHec (x `div` 16) ++ ['C']
              | (mod x 16 == 13) = condectoHec (x `div` 16) ++ ['D']
              | (mod x 16 == 14) = condectoHec (x `div` 16) ++ ['E']
              | (mod x 16 == 15) = condectoHec (x `div` 16) ++ ['F']
              | otherwise = condectoHec (x `div` 16) ++ show (mod x 16) 

{-| EXEMPLO: 26|16   ->    1|16       OS RESTOS: 1 10 = 1A 
             10  1         1 0
|-}

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- |CONVERTE UM BINARIO PARA HEXADECIMAL
menuBintoHec :: IO ()
menuBintoHec = do

        putStr "\n\nDigite um binario: "
        string <- getLine
        imprimeBintoHec string
        saidaBintoHec
        
imprimeBintoHec :: String -> IO ()
imprimeBintoHec string = do

-- | PRIMEIRO CONVERTE O BINARIO PARA DECIMAL
        let decimal = conbintoDec (reverse string) 1 

-- | DEPOIS CONVERTE O DECIMAL PARA HEXADECIMAL
        let hexadecimal = condectoHec decimal 

        putStr "\n"
        putStr string
        putStr "_(2) = "
        putStr hexadecimal
        putStr "_(16)"
        
saidaBintoHec :: IO ()
saidaBintoHec = do 

        putStr "\n\nDeseja converter outro binario (s/n)? "

        x <- getChar

        case x of
            's' -> menuBintoHec
            'n' -> return ()
            _ -> do putStr "\n\nOpcao Invalida"
                    saidaBintoHec

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM HEXADECIMAL PARA BINARIO
menuHectoBin :: IO ()
menuHectoBin = do

        putStr "\n\nDigite um hexadecimal: "
        string <- getLine
        imprimeHectoBin string
        saidaHectoBin
        
imprimeHectoBin :: String -> IO ()
imprimeHectoBin string = do

-- | PRIMEIRO CONVERTE O HEXADECIMAL PARA DECIMAL
        let decimal = conhectoDec (reverse string) 0
 
-- | DEPOIS CONVERTE O DECIMAL PARA BINARIO
        let binario = condectoBin decimal
        
        putStr "\n"
        putStr string
        putStr "_(16) = "
        putStr binario
        putStr "_(2)"
        
saidaHectoBin :: IO ()
saidaHectoBin = do

        putStr "\n\nDeseja converter outro binario (s/n)? "

        x <- getChar

        case x of
            's' -> menuHectoBin
            'n' -> return ()
            _ -> do putStr "\n\nOpcao Invalida"
                    saidaHectoBin

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
menuFbtoDec :: IO ()
menuFbtoDec = do

       putStr "\n\nDigite um binário fracionário: "
       string <- getLine
       imprimeFbtoDec string
       saidaFbtoDec 
       
imprimeFbtoDec :: String -> IO ()
imprimeFbtoDec string = do

-- | ENCONTRA A VIRGULA NA STRING E GUARDA A POSICAO
       let posicaoVirgula = encontravirgula string 1

-- | GUARDA A PARTE INTEIRA DA STRING (ANTES DA VÍRGULA)
       let stringInteira = parteInteira string (posicaoVirgula)

-- | GUARDA A PARTE FRACIONARIA DA STRING (DEPOIS DA VÍRGULA
       let stringFracionaria = parteFracionaria string (posicaoVirgula)

-- | PASSA A PARTE INTEIRA DA STRING PARA O CONVERSOR DE BINARIO PARA DECIMAL
       let inteira = conbintoDec (reverse stringInteira) 1

-- | PASSA A PARTE FRACIONARIA DA STRING PARA O CONVERSOR DE BINARIO FRACIONARIA PARA DECIMAL
       let fracionaria = conbinFractoDec stringFracionaria (0.5)

       putStr "\n"
       putStr string
       putStr "_(2) = "

-- | SOMA AS DUAS CONVERSÕES PARA RESULTAR NO DECIMAL FINAL CONVERTIDO
       putStr (show (fromIntegral inteira + fracionaria))
       putStr "_(10)" 
       
saidaFbtoDec :: IO ()
saidaFbtoDec = do

       putStr "\n\nDeseja converter outro binário fracionário (s/n)? "

       x <- getChar

       case x of
           's' -> menuFbtoDec
           'n' -> return ()
           _ -> do putStr "\n\nOpcao Invalida"
                   saidaFbtoDec 

-- | A FUNCAO CONBINFRACTODEC RECEBE UMA LISTA DE CARACTERES (STRING) E UM VALOR DOUBLE (0.5)
conbinFractoDec :: [Char] -> Double -> Double

conbinFractoDec [] _ = 0

{-| 
   SE A CABEÇA DA LISTA FOR 0, CHAMA A FUNÇÃO RECURSIVAMENTE PASSANDO A LISTA E O VALOR DA CASA DIVIDIDO POR 2
   JÁ SE FOR 1, SOMA O VALOR DA CASA À CHAMADA RECURSIVA PASSANDO A LISTA E O VALOR DA CASA DIVIDIDO POR 2
|-}
conbinFractoDec (x:xs) y | (x == '0') = conbinFractoDec xs (y / 2)
                         | (x == '1') = y + conbinFractoDec xs (y / 2)
                         | otherwise = error "Binário fracionário inválido" 
                         
{-| EXEMPLO	, 0   1   0   = 1/4 = 0.25 
                 1/2 1/4 1/8
|-}

--------------------------

-- | A FUNCAO ENCONTRAVIRGULA RECEBE UMA LISTA DE CARACTERES (STRING) E UM INTEIRO (1) E VOLTA A POSICAO DA VIRGULA OU PONTO
encontravirgula :: [Char] -> Int -> Int 
encontravirgula [] _ = 0
encontravirgula (x:xs) y | (x == ',') || (x == '.') = y
                         | otherwise = encontravirgula xs (y + 1)

--------------------------

-- | A FUNCAO PARTEINTEIRA RECEBE UMA LISTA DE CARACTERES E UM INTEIRO (POSICAO DA VIRGULA) E VOLTA A STRING ANTES DA VIRGULA
parteInteira :: [Char] -> Int -> [Char]
parteInteira [] _ = ['0']

-- | SE A POSICAO DA VIRGULA FOR 0, OU SEJA, NÃO HÁ VÍRGULA NA STRING, VOLTA A STRING INTEIRA
parteInteira (x:xs) y | (y == 0) = (x:xs)
                      | otherwise = take (y - 1) (x:xs)
-- | ^ take(y - 1) NÃO INCLUI A VIRGULA NA STRING

--------------------------

-- | A FUNCAO PARTEFRACIONARIA RECEBE UMA LISTA DE CARACTERES E UM INTEIRO (POSICAO DA VIRGULA) E VOLTA A STRING APOS A VIRGULA
parteFracionaria :: [Char] -> Int -> [Char]

-- | SE A POSICAO DA VIRGULA FOR 0, VOLTA APENAS O CARACTER '0' (',0')
parteFracionaria _ 0 = ['0']

parteFracionaria [] _ = ['0']
parteFracionaria (x:xs) y = drop y (x:xs)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | CONVERTE UM DECIMAL FRACIONARIO PARA BINARIO
menuFdtoBin :: IO ()
menuFdtoBin = do

       putStr "\n\nDigite um real ('.'): "
       string <- getLine
       imprimeFdtoBin string
       saidaFdtoBin
       
imprimeFdtoBin :: String -> IO ()
imprimeFdtoBin string = do

-- | ENCONTRA A VIRGULA
       let posicaoVirgula = encontravirgula string 1

-- | SEPARA A PARTE INTEIRA
       let stringInteira = parteInteira string (posicaoVirgula)

-- | SEPARA A PARTE FRACIONARIA
       let stringFracionaria = parteFracionaria string (posicaoVirgula)

-- | CONVERTE A PARTE INTEIRA PARA BINARIO (PASSANDO O VALOR CONVERTIDO PARA INTEIRO)
       let inteira = condectoBin (read stringInteira :: Int)

-- | CONVERTE A PARTE FRACIONARIA PARA BINARIO. PARA CONSEGUIR APENAS A PARTE FRACIONARIA, FAZ-SE A STRING COMPLETA MENOS A PARTE INTEIRA
       let fracionaria = convertefrac ((read string :: Double) - (read stringInteira :: Double)) 0
 
       putStr "\n"
       putStr string
       putStr "_(10) = "
       putStr inteira
       putStr "," 
       putStr fracionaria
       putStr "_(2)"
       
saidaFdtoBin :: IO ()
saidaFdtoBin = do

       putStr "\n\nDeseja converter outro binário fracionário (s/n)? "

       x <- getChar

       case x of
           's' -> menuFdtoBin
           'n' -> return ()
           _ -> do putStr "\n\nOpcao Invalida"
                   saidaFdtoBin
           
-- | A FUNCAO RECEBE UM DOUBLE (PARTE FRACIONARIA) E UM INTEIRO (NUMERO DE CASAS), E VOLTA UMA LISTA DE CARACTERES (BINARIO FRACIONARIO)
convertefrac :: Double -> Int -> [Char]

-- | CASO BASE 1: 0 RETORNA UMA LISTA COM O CARACTER '0'
convertefrac 0 _ = ['0']

-- | CASO BASE 2: DEFINIDO O NÚMERO MÁXIMO DE 16 CASAS PARA A PARTE FRACIONARIA. SE O NÚMERO É ATINGIDO, VOLTA UMA LISTA VAZIA
convertefrac _ 16 = []

{-| 
   CASO A MULTIPLICAÇÃO DO DOUBLE POR 2 SEJA MAIOR OU IGUAL A 1, ADICIONA O CARACTER 1 NA LISTA E CONCATENA COM A CHAMADA RECURSIVA PASSANDO O DOUBLE MULTIPLICADO POR 2 MENOS A PARTE INTEIRA DO RESULTADO. E INCREMENTA O NUMERO DE CASAS
   SE A MULTIPLICAÇÃO DO DOUBLE POR 2 SEJA MENOR QUE 1, VERIFICA SE O NUMERO MÁXIMO DE CASAS NÃO FOI ATINGIDO. SE NÃO TIVER SIDO ATINGIDO, ADICIONA O CARACTER 0 NA LISTA E CONCATENA COM A MESMA CHAMADA RECURSIVA. JÁ SE O LIMITE TIVER SIDO ATINGIDO, VOLTA LISTA VAZIA
|-}
convertefrac d c | (d * 2 >= 1) = ['1'] ++ convertefrac (d * 2 - fromIntegral(truncate (d * 2))) (c + 1)
                 | otherwise = if (c < 16) then ['0'] ++ convertefrac (d * 2) (c + 1) else []

{-| EXEMPLO:  0.625     ->    0.25     ->    0.5         0
              x   2           x  2           x 2       x 2
             -------         ------         -----     ------
               1,25            0.5             1         0         -> 0, 1 0 1 0
|-}

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | SOMA DOIS BINARIOS SEM SINAL
menuSomaBin :: IO ()
menuSomaBin = do

{-| COM INTUITO DE CONSEGUIR FAZER A SOMA DE DOIS BINARIOS INTEIROS, DOIS BINARIOS FRACIONARIOS E DE UM BINARIO INTEIRO E UM FRACIONARIO, A FUNCAO TRATA TODOS OS 
    BINARIOS COMO BINARIOS FRACIONARIOS, POR EXEMPLO, 1011 = 1011.0
|-}

       putStr "\n\nDigite o primeiro binário: "
       primeirobinario <- getLine
       putStr "\nDigite o segundo binário: "
       segundobinario <- getLine
       imprimeSomaBin primeirobinario segundobinario
       saidaSomaBin
       
imprimeSomaBin :: String -> String -> IO ()
imprimeSomaBin primeirobinario segundobinario = do

-- | "TRATAMENTO" DO PRIMEIRO BINARIO. MESMO QUE SEJA UM BINARIO INTEIRO, A FUNCAO IRÁ CONVERTE-LO PARA UM DECIMAL FRACIONARIO

       let virgulabinario1 = encontravirgula primeirobinario 1

       let stringInteiraBinario1 = parteInteira primeirobinario virgulabinario1

       let stringFracionariaBinario1 = parteFracionaria primeirobinario virgulabinario1

       let inteirabinario1 = conbintoDec (reverse stringInteiraBinario1) 1

       let fracionariabinario1 = conbinFractoDec (stringFracionariaBinario1) 0.5

-- | "TRATAMENTO" DO SEGUNDO BINARIO.

       let virgulabinario2 = encontravirgula segundobinario 1

       let stringInteiraBinario2 = parteInteira segundobinario virgulabinario2

       let stringFracionariaBinario2 = parteFracionaria segundobinario virgulabinario2

       let inteirabinario2 = conbintoDec (reverse stringInteiraBinario2) 1

       let fracionariabinario2 = conbinFractoDec (stringFracionariaBinario2) 0.5
       

       let resultadoDecimal = ((fromIntegral inteirabinario1) + fracionariabinario1) + ((fromIntegral inteirabinario2) + fracionariabinario2)

       let resultadoBinario = show (resultadoDecimal)

       let virgularesultado = encontravirgula resultadoBinario 1

       let stringInteiraResultado = parteInteira resultadoBinario virgularesultado

       let stringFracionariaResultado = parteFracionaria resultadoBinario virgularesultado

       let inteiraResultado = condectoBin (read stringInteiraResultado :: Int)

       let fracionariaResultado = convertefrac (resultadoDecimal - (read stringInteiraResultado :: Double)) 0

       putStr "\n"

       putStrLn $ "\t" ++ primeirobinario
       putStrLn $ "+" ++ "\t" ++ segundobinario
       putStrLn "-------------------------" 
       putStrLn $ "\t" ++ inteiraResultado ++ "," ++ fracionariaResultado ++ "_(2)"

       putStr "\n"

       putStrLn $ "\t" ++ (show (fromIntegral inteirabinario1 + fracionariabinario1))
       putStrLn $ "+" ++ "\t" ++ (show (fromIntegral inteirabinario2 + fracionariabinario2))
       putStrLn "-------------------------" 

       putStrLn $ "\t" ++ resultadoBinario ++ "_(10)"
       
saidaSomaBin :: IO ()
saidaSomaBin = do
       
       putStr "\n\nDeseja somar outros binários (s/n)? "

       x <- getChar

       case x of
           's' -> menuSomaBin
           'n' -> return ()
           _ -> do putStr "\n\nOpcao Invalida"
                   saidaSomaBin
           
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
menu :: IO ()
menu = do

    putStr "\n\n\n"
    putStrLn "**************************************************"
    putStrLn "*\t\tC o n v e r s o r\t\t *"
    putStrLn "**************************************************"

    putStrLn "\nOpcoes:\n"

    putStrLn "1 - Converter de binário para decimal"
    putStrLn "2 - Converter de hexadecimal para decimal"
    putStrLn "3 - Converter de decimal para binario"
    putStrLn "4 - Converter de decimal para hexadecimal"
    putStrLn "5 - Converter de binário para hexadecimal"
    putStrLn "6 - Converter de hexadecimal para binário"
    putStrLn "7 - Converter de fração binária para decimal"
    putStrLn "8 - Converter de fração decimal para binário"
    putStrLn "9 - Somar dois binários sem sinal"
    putStrLn "0 - Sair\n"

    opcao <- lerOpcao 0 9
    lacoMenu opcao

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
lerOpcao :: Int -> Int -> IO Int

lerOpcao min max = do putStr "Digite uma opcao: "
                      opcao <- readLn
                      valida opcao
   where
     
     valida opcao | min <= opcao && opcao <= max = return opcao
                  | otherwise = do putStrLn "\nOpcao invalida!\n"
                                   lerOpcao min max
                                   
----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
lacoMenu :: Int -> IO ()

lacoMenu opcao =

    case opcao of
    
       1 -> do menuBintoDec
               menu
       2 -> do menuHectoDec 
               menu
       3 -> do menuDectoBin
               menu
       4 -> do menuDectoHec
               menu
       5 -> do menuBintoHec
               menu
       6 -> do menuHectoBin
               menu
       7 -> do menuFbtoDec
               menu
       8 -> do menuFdtoBin
               menu
       9 -> do menuSomaBin
               menu
       0 -> encerra

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
main :: IO ()
main = do limpaBuffer
          menu
          
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
limpaBuffer :: IO ()
limpaBuffer = do hSetBuffering stdin NoBuffering

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
encerra :: IO ()
encerra = do putStrLn "\n\nSaindo...\n\n"
