{-|
Aluno: Malcon Rezende Rodrigues
Matrícula: 12211BCC034
Segundo Trabalho Programação Funcional
|-}

-- | Biblioteca de Funções IO
import System.IO (IOMode(WriteMode, ReadMode), openFile, stdout, stdin, hSetBuffering, BufferMode(NoBuffering), BufferMode(LineBuffering), hClose, readFile)
import System.Directory (doesFileExist)
import Data.List (sort)

-- | Estrutura para informações de um estudante. Armazena notas como Floats para contemplar notas decimais
data Estudantes = Estudante {
     nome :: String,
     matricula :: String,
     p1 :: Float,
     p2 :: Float,
     p3 :: Float,
     t1 :: Float,
     t2 :: Float,
     faltas :: Int,
     notafinal :: Float,
     situacao :: Char
     } deriving (Show)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Menu de Leitura de Arquivo - Recebe uma string (nome do arquivo) e devolve uma string (nome do arquivo atualizado)
menuLerTurma :: String -> IO String
menuLerTurma arquivo = do

    exibirOpcoes texto
    opcao <- lerOpcao 1 3

-- | Recebe o novo nome do arquivo e o retorna
    novoarquivo <- processarOpcao' opcao arquivo
    return novoarquivo

        where 
           texto =  [ "========================================",
                      "                Ler Turma               ",
                      "========================================",
                      "Opcoes:\n",
                      "1 - Ler turma existente",
                      "2 - Criar nova turma",
                      "3 - Voltar ao menu principal" ]


-- | Processa Opções do Menu de Leitura. Recebe da função existeTurma ou novaTurma o novo nome do arquivo
processarOpcao' :: Int -> String -> IO String
processarOpcao' opcao arquivo
    | (opcao == 1) = do novoarquivo <- existeTurma
                        menuLerTurma novoarquivo -- | Retorna o nome do arquivo
    | (opcao == 2) = do novoarquivo <- novaTurma
                        menuLerTurma novoarquivo -- | Retorna o nome do arquivo
    | (opcao == 3) = do return arquivo

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para saber se o arquivo já existe ou não
existeTurma :: IO String
existeTurma = do 
   
    putStr "\nDigite o nome do arquivo com a turma: "
    arquivo <- getLine

-- | A função "doesFileExist", da biblioteca System Directory, verifica se um arquivo existe e retorna um valor booleano (1- existe / 2 - não existe)
    existe <- doesFileExist arquivo

-- | Caso o arquivo exista, passa o arquivo para a função lerTurma, caso não exista, retorna uma string vazia
    if existe 
        then do putStr ("\nArquivo " ++ arquivo ++ " lido com sucesso!")

-- Já faz a leitura do arquivo para que, caso o arquivo tenha um formato inválido, apontará o erro no momento de leitura
                lerTurma arquivo
                return arquivo
        else do putStr ("Arquivo " ++ arquivo ++ " não existe!")
                return ""

-- | Função para ler o arquivo. Recebe o nome do arquivo e devolve uma lista de estruturas
lerTurma :: FilePath -> IO [Estudantes]
lerTurma arquivo = do

-- | A função "readFile" lê todo o conteúdo de um arquivo para uma única string
    dados <- readFile arquivo

-- | A função lines "quebrará" a grande string em strings menores, dividindo a cada "\n" encontrado
    let linhas = lines dados

-- | Passa a lista de strings para a função guardaestrutura e guarda seu retorno, que será uma lista de estruturas, em estudantes
    let estudantes = guardaestrutura linhas
    return estudantes

-- | Função para guardar a lista de strings em uma lista de estruturas
guardaestrutura :: [String] -> [Estudantes]

-- | Caso Base - Caso a string tenha acabado, retorna uma lista vazia
guardaestrutura [] = []

{-| Divide o conteúdo da string para os campos da estrutura. Caso algum dos campos falte ou seja preenchido incorretamente, devolve um erro, indicando que o arquivo tem um
    formato inválido |-}

guardaestrutura (nome:matricula:p1:p2:p3:t1:t2:faltas:notafinal:situacao:resto) = 
                  Estudante nome matricula (read p1) (read p2) (read p3) (read t1) (read t2) (read faltas :: Int) (read notafinal) (head situacao) : guardaestrutura resto
guardaestrutura _ = error "Formato de arquivo inválido"

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para criar um novo arquivo ou substituir um já existente
novaTurma :: IO String
novaTurma = do

    putStr "\nDigite o nome do arquivo para a nova turma: "
    arquivo <- getLine
    existe <- doesFileExist arquivo

-- | Verifica se o arquivo já existe. Caso sim, chama a função subArq passando o nome do arquivo. Caso não, cria um novo arquivo com o nome passado e retorna esse nome
    if existe
        then do novoarquivo <- subArq arquivo 
                return novoarquivo -- | Retorna o nome do arquivo que foi substituído
        else do handle <- openFile arquivo WriteMode
                hClose handle
                putStr ("Arquivo " ++ arquivo ++ " criado com sucesso")
                return arquivo

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para substituir um arquivo existente e devolve o nome desse arquivo
subArq :: String -> IO String
subArq arquivo = do

{-| Faz uma leitura de inteiros para as opções sim ou não por causa de um erro ao ler Char
    erro = User *** Exception: user error (Prelude.readIO: no parse)
|-}

    putStr ("\nArquivo " ++ arquivo ++ " já existe. Deseja sobrescrevê-lo (1 - sim / 2 - nao)?")

    -- putStr ("\nArquivo " ++ arquivo ++ " já existe. Deseja sobrescrevê-lo (s/n)?")
    -- opcao <- lerOpcao'

    opcao <- lerOpcao''

    case opcao of

-- | Caso a opcão seja de substituir, abre o arquivo com limpeza de conteúdo e retorna o nome
        1 -> do handle <- openFile arquivo WriteMode
                hClose handle
                return arquivo
-- | Caso a opção seja de não substituir, retorna uma string vazia (nenhum arquivo foi lido)
        2 -> return ""

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para imprimir a turma de estudantes. Recebe uma string (nome do arquivo) e uma lista de estruturas (turma)
imprimirTurma :: String -> [Estudantes] -> IO ()
imprimirTurma arquivo estudantes = 

-- | Caso o arquivo não exista (é uma string vazia) imprime a mensagem de que não há uma turma carregada
    if (arquivo == "") then putStr "\nNão há turma carregada!\n"

{-| Caso o arquivo exista, imprime a turma utilizando a função mapM_, que enquanto a lista de estruturas não estiver vazia, imprimira as informações com a função printTurma
    A função mapM_ é uma combinação das funções map e sequence_, onde a função sequence_ executa em ordem uma lista de ações descartando seus resultados
|-}
                       else do putStrLn "\n\t\t\t\tEstudantes"
                               putStrLn "-----------------------------------------------------------------------------------"
                               putStrLn "Nome                          Matrícula      N1   N2   N3   T1   T2   Fal Final Sit"
                               putStrLn "-----------------------------------------------------------------------------------"
                               mapM_ printTurma estudantes

-- | Imprimir estudante, recebe uma estrutura por vez e imprime suas informações    
printTurma :: Estudantes -> IO ()
printTurma estudante = do

-- | Utiliza a função makespaces, que dado o tamanho da string, imprime o número de espaços necessários para garantir a indentação

    let tamanhonome = makespaces (30 - (length (nome estudante)))
    let tamanhomatricula = makespaces (15 - (length (matricula estudante)))
    let tamanhonota1 = makespaces (5 - length (show (p1 estudante)))
    let tamanhonota2 = makespaces (5 - length (show (p2 estudante)))
    let tamanhonota3 = makespaces (5 - length (show (p3 estudante)))
    let tamanhotrabalho1 = makespaces (5 - length (show (t1 estudante)))
    let tamanhotrabalho2 = makespaces (5 - length (show (t2 estudante)))
    let tamanhofaltas = makespaces (4 - length (show (faltas estudante)))
    let tamanhofinal = makespaces (6 - length (show (notafinal estudante)))

-- | Imprime as informações do estudante em uma única linha
    putStrLn (nome estudante ++ tamanhonome ++ matricula estudante ++ tamanhomatricula ++ show (p1 estudante) ++ tamanhonota1 ++ show (p2 estudante) ++ tamanhonota2 ++ show (p3 estudante) ++ tamanhonota3 ++ show (t1 estudante) ++ tamanhotrabalho1 ++ show (t2 estudante) ++ tamanhotrabalho2 ++ show (faltas estudante) ++ tamanhofaltas ++ show (notafinal estudante) ++ tamanhofinal ++ [situacao estudante])

----------------------------------------------------------------------------------------------------------------------------------------------------------------------       
-- | Função para imprimir as estatísticas de uma turma de estudantes. Recebe uma string (nome do arquivo) e uma lista de estruturas (turma)
imprimirEstatistica :: String -> [Estudantes] -> IO ()
imprimirEstatistica arquivo estudantes = 

-- | Caso o arquivo não exista (é uma string vazia) imprime a mensagem de que não há uma turma carregada
    if (arquivo == "") then putStr "\nNão há turma carregada!\n"

-- | Caso o arquivo exista, chama a função printEstatítica passando a lista de estruturas (turma)
                       else do putStrLn "\n\t\t\tEstatísticas"
                               putStrLn "-------------------------------------------------------------"
                               putStrLn "                          N1    N2    N3    T1    T2    Final"
                               putStrLn "-------------------------------------------------------------"
                               printEstatistica estudantes

-- | Função para imprimir as estatísticas de uma turma de estudantes. Recebe uma lista de estruturas (turma)
printEstatistica :: [Estudantes] -> IO ()

-- | Acabado a lista de estruturas, retorna nada
printEstatistica [] = return ()


printEstatistica estudantes = do

-- | Calculo de maior nota. Chama a função maiornota passando a lista de estudantes e um modo (1 = prova 1, 2 = prova 2, 3 = prova 3, 4 = trabalho 1, ...). Guarda o resultado em man1 (maior nota 1, 2, 3..)
    let man1 = maiornota estudantes 1
    let man2 = maiornota estudantes 2
    let man3 = maiornota estudantes 3
    let mat1 = maiornota estudantes 4
    let mat2 = maiornota estudantes 5
    let manf = maiornota estudantes 6

-- | Calculo de menor nota. Chama a função menornota passando a lista de estudantes e um modo. Guarda o resultado em men1 (menor nota 1, 2, 3..)
    let men1 = menornota estudantes 1
    let men2 = menornota estudantes 2
    let men3 = menornota estudantes 3
    let met1 = menornota estudantes 4
    let met2 = menornota estudantes 5
    let menf = menornota estudantes 6

-- | Calculo das medias de cada nota. Chama a função medianota passando a lista de estudantes e o valor 0 para a soma e a quantidade
    let mdn1 = medianota estudantes 1 0 0
    let mdn2 = medianota estudantes 2 0 0
    let mdn3 = medianota estudantes 3 0 0
    let mdt1 = medianota estudantes 4 0 0 
    let mdt2 = medianota estudantes 5 0 0 
    let mdnf = medianota estudantes 6 0 0

-- | Calculo das medianas de cada nota. Chama a função mediananota passando uma lista ordenada (notas p1, p2, p3, t1, t2, nota final)
    let medn1 = mediananota (sort (separaLista estudantes 1))
    let medn2 = mediananota (sort (separaLista estudantes 2))
    let medn3 = mediananota (sort (separaLista estudantes 3))
    let medt1 = mediananota (sort (separaLista estudantes 4))
    let medt2 = mediananota (sort (separaLista estudantes 5))
    let mednf = mediananota (sort (separaLista estudantes 6))


-- | Quantidade de estudantes aprovados, reprovados ou reprovados por falta. Chama a função encontrasituacao passando a lista de estruturas e um modo (1 - aprovados, 2 - reprovados, 3 - reprovados por falta)
    let aprovados = encontrasituacao estudantes 1
    let reprovados = encontrasituacao estudantes 2
    let reprovadosF = encontrasituacao estudantes 3

{-|
    Calculo da porcentagem de alunos aprovados, reprovados ou reprovados por falta. Divide o número de alunos da situação pelo total de alunos, guardando apenas uma casa após a vírgula
    Exemplo:      (truncate (10 * (3 / 9) * 100))) / 10 = 33.3
                    
|-}
    let totalaprovados = (fromIntegral (truncate (10 * (fromIntegral aprovados / fromIntegral (totalalunos estudantes)) * 100))) / 10
    let totalreprovados = (fromIntegral (truncate (10 * (fromIntegral reprovados / fromIntegral (totalalunos estudantes)) * 100))) / 10
    let totalreprovadosF = (fromIntegral (truncate (10 * (fromIntegral reprovadosF / fromIntegral (totalalunos estudantes)) * 100))) / 10

-- | Calculo dos intervalos. Chama a função histograma passando uma lista de estruturas e o intervalo desejado
    let intervalo1 = histograma estudantes 0 10
    let intervalo2 = histograma estudantes 11 20
    let intervalo3 = histograma estudantes 21 30
    let intervalo4 = histograma estudantes 31 40
    let intervalo5 = histograma estudantes 41 50
    let intervalo6 = histograma estudantes 51 60
    let intervalo7 = histograma estudantes 61 70
    let intervalo8 = histograma estudantes 71 80
    let intervalo9 = histograma estudantes 81 90
    let intervalo10 = histograma estudantes 91 100

-- | Calculo dos espaços para garantir a indentação. Cada valor pode ocupar 6 espaços
    let tamanhoman1 = makespaces (6 - (length (show man1)))
    let tamanhoman2 = makespaces (6 - (length (show man2)))
    let tamanhoman3 = makespaces (6 - (length (show man3)))
    let tamanhomat1 = makespaces (6 - (length (show mat1)))
    let tamanhomat2 = makespaces (6 - (length (show mat2)))
    let tamanhomanf = makespaces (6 - (length (show manf)))

    let tamanhomen1 = makespaces (6 - (length (show men1)))
    let tamanhomen2 = makespaces (6 - (length (show men2)))
    let tamanhomen3 = makespaces (6 - (length (show men3)))
    let tamanhomet1 = makespaces (6 - (length (show met1)))
    let tamanhomet2 = makespaces (6 - (length (show met2)))
    let tamanhomenf = makespaces (6 - (length (show menf)))

    let tamanhomdn1 = makespaces (6 - (length (show mdn1)))
    let tamanhomdn2 = makespaces (6 - (length (show mdn2)))
    let tamanhomdn3 = makespaces (6 - (length (show mdn3)))
    let tamanhomdt1 = makespaces (6 - (length (show mdt1)))
    let tamanhomdt2 = makespaces (6 - (length (show mdt2)))
    let tamanhomdnf = makespaces (6 - (length (show mdnf)))

    let tamanhomedn1 = makespaces (6 - (length (show medn1)))
    let tamanhomedn2 = makespaces (6 - (length (show medn2)))
    let tamanhomedn3 = makespaces (6 - (length (show medn3)))
    let tamanhomedt1 = makespaces (6 - (length (show medt1)))
    let tamanhomedt2 = makespaces (6 - (length (show medt2)))
    let tamanhomednf = makespaces (6 - (length (show mednf)))


-- | Impressão de maiores notas, menores notas, médias e medianas. Imprime as informações em uma única linha
    putStrLn ("Maiores notas da turma    " ++ (show man1) ++ tamanhoman1 ++ (show man2) ++ tamanhoman2 ++ (show man3) ++ tamanhoman3 ++ (show mat1) ++ tamanhomat1 ++ (show mat2) ++ tamanhomat2 ++ (show manf) ++ tamanhomanf)
    putStrLn ("Menores notas da turma    " ++ (show men1) ++ tamanhomen1 ++ (show men2) ++ tamanhomen2 ++ (show men3) ++ tamanhomen3 ++ (show met1) ++ tamanhomet1 ++ (show met2) ++ tamanhomet2 ++ (show menf) ++ tamanhomenf)
    putStrLn ("Notas médias da turma     " ++ (show mdn1) ++ tamanhomdn1 ++ (show mdn2) ++ tamanhomdn2 ++ (show mdn3) ++ tamanhomdn3 ++ (show mdt1) ++ tamanhomdt1 ++ (show mdt2) ++ tamanhomdt2 ++ (show mdnf) ++ tamanhomdnf)
    putStrLn ("Notas medianas da turma   " ++ (show medn1) ++ tamanhomedn1 ++ (show medn2) ++ tamanhomedn2 ++ (show medn3) ++ tamanhomedn3 ++ (show medt1) ++ tamanhomedt1 ++ (show medt2) ++ tamanhomedt2 ++ (show mednf) ++ tamanhomednf)
    putStrLn "-------------------------------------------------------------"
    
    putStrLn ""

-- | Impressão da quantidade e porcentagem de alunos aprovados, reprovados e reprovados por falta
    putStrLn ("Número de estudantes aprovados:             " ++ (show aprovados) ++ " (" ++ (show totalaprovados) ++ " %)")
    putStrLn ("Número de estudantes reprovados:            " ++ (show reprovados) ++ " (" ++ (show totalreprovados) ++ " %)")
    putStrLn ("Número de estudantes reprovados por falta:  " ++ (show reprovadosF) ++ " (" ++ (show totalreprovadosF) ++ " %)")

    putStrLn ""

-- | Impressão do histograma com intervalos indo de 0 a 100
    putStrLn ("Histograma de notas finais em grupos de 10 pontos:\n")
    putStrLn ("0 - 10    " ++ show intervalo1)
    putStrLn ("11 - 20   " ++ show intervalo2)
    putStrLn ("21 - 30   " ++ show intervalo3)
    putStrLn ("31 - 40   " ++ show intervalo4)
    putStrLn ("41 - 50   " ++ show intervalo5)
    putStrLn ("51 - 60   " ++ show intervalo6)
    putStrLn ("61 - 70   " ++ show intervalo7)
    putStrLn ("71 - 80   " ++ show intervalo8)
    putStrLn ("81 - 90   " ++ show intervalo9)
    putStrLn ("91 - 100  " ++ show intervalo10)


-- | FUNÇÕES PARA A ESTATÍSTICA

-- | Função para calcular os intervalos do Histograma. Recebe uma lista de estruturas, o mínimo e o máximo do intervalo
histograma :: [Estudantes] -> Float -> Float -> Int

-- | Caso Base: caso a lista de estruturas esteja vazia, retorna 0
histograma [] _ _ = 0

-- | Verifica se a nota final do estudante está no intervalo. Se sim, soma 1 a chamada recursiva passando o resto da lista de estruturas, o mínimo e o máximo
histograma (estudante:resto) min max | ((notafinal estudante >= min) && (notafinal estudante <= max)) = 1 + histograma resto min max
                                     | otherwise = histograma resto min max -- Se não, chama recursivamente a função passando o resto da lista de estruturas, o mínimo e o máximo

-- | Função para encontrar a situação de uma estudante dado um modo. Recebe a lista de estruturas, o modo e retorna a quantidade
encontrasituacao :: [Estudantes] -> Int -> Int
encontrasituacao [] _ = 0
encontrasituacao (estudante:resto) n | (n == 1) && (situacao estudante == 'A') = 1 + encontrasituacao resto n
                                     | (n == 2) && (situacao estudante == 'R') = 1 + encontrasituacao resto n
                                     | (n == 3) && (situacao estudante == 'F') = 1 + encontrasituacao resto n
                                     | otherwise = encontrasituacao resto n

-- | Função para calcular o total de alunos. Recebe a lista de estruturas e volta a quantidade
totalalunos :: [Estudantes] -> Int
totalalunos [] = 0
totalalunos (estudante:resto) = 1 + totalalunos resto

-- | Função para calcular a mediana. Recebe uma lista de floats ordenada (pode ser a lista das notas da prova 1, 2, 3, trabalho 1, 2 ou das notas finais) e volta o valor da mediana
mediananota :: [Float] -> Float

-- | Caso base: se a lista de floats estiver vazia, volta o valor float 0.0
mediananota [] = 0.0

{-| 
    Se a lista não estiver vazia, calcula o tamanho da lista e a posição do meio da lista.
    Caso o tamanho da lista não seja par, volta o valor do meio da lista como o valor da mediana.
    Caso o tamanho da lista seja par, pega os dois valores do meio da lista, soma e divide por dois, o resultado é a mediana
|-}
mediananota lista 
    | (tamanho `mod` 2 /= 0) = lista !! meio
    | otherwise = (lista !! (meio - 1) + lista !! meio) / 2
    where
        tamanho = length lista
        meio = tamanho `div` 2

-- | Função para separar as notas da estrutura para uma lista. Recebe a lista de estruturas, um modo e devolve uma lista de floats
separaLista :: [Estudantes] -> Int -> [Float]

-- | Caso Base: se a lista de estruturas estiver vazia, volta uma lista vazia
separaLista [] _ = []

-- | De acordo com o modo passado, pega o valor da nota da estrutura e coloca na lista de floats
separaLista (estudante:resto) n | (n == 1) = (p1 estudante) : separaLista resto n
                                | (n == 2) = (p2 estudante) : separaLista resto n
                                | (n == 3) = (p3 estudante) : separaLista resto n
                                | (n == 4) = (t1 estudante) : separaLista resto n
                                | (n == 5) = (t2 estudante) : separaLista resto n
                                | (n == 6) = (notafinal estudante) : separaLista resto n

-- Função para encontrar a maior nota na lista de estruturas dado um modo. Recebe uma lista de estruturas e um modo
maiornota :: [Estudantes] -> Int -> Float

{-| 
   A função maximum retorna o maior elemento em uma lista. 
   Maximum é aplicado em uma lista resultante da compreensão de lista. 
   A compreensão de lista criará uma lista contendo todas as notas de cada estudante da lista de estruturas
|-}
maiornota estudantes n | (n == 1) = maximum [p1 estudante | estudante <- estudantes]
                       | (n == 2) = maximum [p2 estudante | estudante <- estudantes]
                       | (n == 3) = maximum [p3 estudante | estudante <- estudantes]
                       | (n == 4) = maximum [t1 estudante | estudante <- estudantes]
                       | (n == 5) = maximum [t2 estudante | estudante <- estudantes]
                       | (n == 6) = maximum [notafinal estudante | estudante <- estudantes]

{-| 
   A função minimum retorna o menor elemento em uma lista. 
   Minimum é aplicado em uma lista resultante da compreensão de lista. 
   A compreensão de lista criará uma lista contendo todas as notas de cada estudante da lista de estruturas
|-}
menornota :: [Estudantes] -> Int -> Float
menornota estudantes n | (n == 1) = minimum [p1 estudante | estudante <- estudantes]
                       | (n == 2) = minimum [p2 estudante | estudante <- estudantes]
                       | (n == 3) = minimum [p3 estudante | estudante <- estudantes]
                       | (n == 4) = minimum [t1 estudante | estudante <- estudantes]
                       | (n == 5) = minimum [t2 estudante | estudante <- estudantes]
                       | (n == 6) = minimum [notafinal estudante | estudante <- estudantes]

-- | Função para calcular a média das notas. Recebe uma lista de estruturas, um modo, a soma (igual a 0), a quantidade (igual a 0) e devolve um float (média)
medianota:: [Estudantes] -> Int -> Float -> Int -> Float

{-| 
    Caso Base: se a lista estiver vazia, verifica o valor de qtd
    Se qtd for igual a 0, a lista de estruturas é vazia, retorna 0
    Se qtd for diferente de 0, significa que a lista acabou, então calcula-se a média garantindo que o valor terá apenas uma casa decimal
|-}
medianota [] _ soma qtd = if (qtd == 0) then 0 else fromIntegral (truncate (10 * soma / fromIntegral qtd)) / 10

-- | Caso a lista não seja vazia, verifica-se o modo e então chama recursivamente a função passando o resto da lista, o modo, a soma acrescido do valor da nota e quantidade mais 1
medianota (estudante:resto) n soma qtd | (n == 1) = medianota resto n (soma + (p1 estudante)) (qtd + 1)
                                       | (n == 2) = medianota resto n (soma + (p2 estudante)) (qtd + 1)
                                       | (n == 3) = medianota resto n (soma + (p3 estudante)) (qtd + 1)
                                       | (n == 4) = medianota resto n (soma + (t1 estudante)) (qtd + 1)
                                       | (n == 5) = medianota resto n (soma + (t2 estudante)) (qtd + 1)
                                       | (n == 6) = medianota resto n (soma + (notafinal estudante)) (qtd + 1)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para cadastrar um novo estudante. Recebe uma string (nome do arquivo), a lista de estruturas e devolve uma lista de estruturas com o novo estudante
cadastrarEstudante :: String -> [Estudantes] -> IO [Estudantes]
cadastrarEstudante arquivo estudantes = 

-- | Caso o arquivo não exista (é uma string vazia), imprime a mensagem de que não há uma turma carregada e retorna a lista de estruturas
    if (arquivo == "") then do putStr "\nNão há turma carregada!\n"
                               return estudantes

-- | Caso o arquivo exista, chama a função cadastrar, guarda seu resultado em uma estrutura e retorna a lista de estudantes concatenado com a nova estrutura (novo estudante)
                       else do putStrLn "========================================"
                               putStrLn "\tCadastrar Novo Estudante"
                               putStrLn "========================================"
                               novoEstudante <- cadastrar
                               return (estudantes ++ [novoEstudante])

-- | Função para "cadastrar" um novo estudante em uma estrutura
cadastrar :: IO Estudantes
cadastrar = do

    putStr "Digite o nome: "
    nome <- getLine
    putStr "Digite a matrícula: "
    matricula <- getLine
    putStrLn "Digite as três notas das provas:"
    p1 <- readLn
    p2 <- readLn
    p3 <- readLn
    putStrLn "Digite as duas notas dos trabalhos: "
    t1 <- readLn
    t2 <- readLn
    putStr "Digite o número de faltas: "
    faltas <- readLn

-- | Chama a função calculaNotaFinal passando as notas do estudante
    let notaFinal = calculaNotaFinal p1 p2 p3 t1 t2

-- | Chama a função calculaSituacao passando as faltas e a nota final já calculada do estudante
    let situacao = calculaSituacao faltas notaFinal

-- | Imprime a nota final e a situação
    putStrLn ("\nNota final calculada: " ++ show notaFinal)
    putStrLn ("Situação final: " ++ [situacao])
    putStr "\n\n"

-- | Retorna a nova estrutura (novo estudante)
    return (Estudante nome matricula p1 p2 p3 t1 t2 faltas notaFinal situacao)


-- | Função para calcular a situação final do estudante
calculaNotaFinal :: Float -> Float -> Float -> Float -> Float -> Float
calculaNotaFinal p1 p2 p3 t1 t2 = (p1 + p2 + p3 + t1 + t2)

-- | Função para calcular a situação do estudante
calculaSituacao :: Int -> Float -> Char
calculaSituacao faltas notaFinal | (faltas <= 18) && (notaFinal >= 60) = 'A'
                                 | (faltas <= 18) && (notaFinal < 60) = 'R'
                                 | otherwise = 'F'

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Menu para encontrar um estudante na lista de estruturas. Recebe uma string (nome do arquivo), a lista de estruturas e volta uma lista de estruturas editada
menuEncontraEstudante :: String -> [Estudantes] -> IO [Estudantes]
menuEncontraEstudante arquivo estudantes = do

-- | Caso o arquivo não exista (é uma string vazia), imprime a mensagem de que não há uma turma carregada e retorna a lista de estruturas
    if (arquivo == "") then do putStr "\nNão há turma carregada!\n"
                               return estudantes

-- | Caso o arquivo exista, chama a função processarOpcao'' passando a opcão desejada e a lista de estruturas e guardando seu resultado em uma lista de estruturas editada
                       else do exibirOpcoes texto
                               opcao <- lerOpcao 1 3
                               novoEstudantes <- processarOpcao'' opcao estudantes
                               return novoEstudantes
                                   
        where 
           texto =  [ "========================================",
                      "     Editar Informações do Estudante    ",
                      "========================================",
                      "Opcoes:\n",
                      "1 - Selecionar por número de matrícula",
                      "2 - Selecionar por nome",
                      "3 - Voltar ao menu principal" ]

-- | Processa Opções do Menu de Encontrar Estudante. Dado a opção do usuário, receberá a lista de estruturas editada da função procuraMatricula ou procuraNome
processarOpcao'' :: Int -> [Estudantes] -> IO [Estudantes]
processarOpcao'' opcao estudantes
    | (opcao == 1) = do novoEstudantes <- procuraMatricula estudantes
                        return novoEstudantes -- | Retorna a lista de estruturas editada

    | (opcao == 2) = do novoEstudantes <- procuraNome estudantes
                        return novoEstudantes -- | Retorna a lista de estruturas editada

    | (opcao == 3) = return estudantes -- | Retorna a lista de estruturas

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para procurar um estudante pela matrícula. Recebe uma lista de estruturas e volta uma lista de estruturas editada
procuraMatricula :: [Estudantes] -> IO [Estudantes]
procuraMatricula estudantes = do

    putStr "Digite o número de matrícula: "
    matricula <- getLine

-- | Chama a função encontraEstudante passando a matrícula, o modo e a lista de estruturas
    let encontrado = encontraEstudante matricula 1 estudantes

    case encontrado of

-- | Caso o estudante tenha sido encontrado (Just estudante), chama a função menuEditaEstudante, passando o estudante encontrado e a lista de estruturas, e guardando seu resultado em novoEstudantes (lista de estruturas editada)
        Just estudante -> do novoEstudantes <- menuEditaEstudante estudante estudantes
                             return novoEstudantes

-- | Caso o estudante não tenha sido encontrado (Nothing), imprime a mensagem "Número de matrícula inválido" e retorna a mesma lista de estruturas
        Nothing -> do putStrLn "\nNúmero de matrícula inválido!"
                      return estudantes

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para procurar um estudante pelo nome. Recebe uma lista de estruturas e volta uma lista de estruturas editada
procuraNome :: [Estudantes] -> IO [Estudantes]
procuraNome estudantes = do

    putStr "Digite nome do estudante: "
    nome <- getLine

-- | Chama a função encontraEstudante passando o nome, o modo e a lista de estruturas
    let encontrado = encontraEstudante nome 2 estudantes

    case encontrado of

-- | Caso o estudante tenha sido encontrado (Just estudante), chama a função menuEditaEstudante, passando o estudante encontrado e a lista de estruturas, e guardando seu resultado em novoEstudantes (lista de estruturas editada)
        Just estudante -> do novoEstudantes <- menuEditaEstudante estudante estudantes
                             return novoEstudantes

-- | Caso o estudante não tenha sido encontrado (Nothing), imprime a mensagem "Nome de Estudante inválido" e retorna a mesma lista de estruturas
        Nothing -> do putStrLn "\nNome de Estudante inválido!"
                      return estudantes

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para encontrar um estudante na lista de estruturas. Recebe uma string (nome ou matrícula), o modo e uma lista de estruturas. Caso encontre (Maybe) retorna uma estrutura (estudante)
encontraEstudante :: String -> Int -> [Estudantes] -> Maybe Estudantes

-- | Caso Base: se a lista de estruturas estiver vazia, retorna Nothing (estudante não encontrado)
encontraEstudante _ _ [] = Nothing

{-|
    Enquanto a lista de estruturas não estiver vazia, de acordo com o modo, procura pela a chave (nome ou matrícula) na lista de estruturas.
    Se encontrar, retorna o estudante ao qual a chave casou (Just estudante).
    Se não encontrar, até que a lista termine, chama recursivamente a função, passando a chave, o modo e o resto da lista de estruturas.
|-}
encontraEstudante chave opcao (estudante:resto) | (opcao == 1) && (chave == matricula estudante) = Just estudante
                                                | (opcao == 2) && (chave == nome estudante) = Just estudante
                                                | otherwise = encontraEstudante chave opcao resto

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Menu para editar um estudante. Recebe uma estrutura (estudante a ser editado), uma lista de estruturas e volta uma lista de estruturas editada
menuEditaEstudante :: Estudantes -> [Estudantes] -> IO [Estudantes]
menuEditaEstudante estudante estudantes = do


    exibirOpcoes texto
    opcao <- lerOpcao 1 9

-- | Dado a opção desejada pelo usuário, chama a função editaEdudante passando a opção e a estrutura (estudante a ser editado) e guarda em novoEstudante
    novoEstudante <- editaEstudante opcao estudante

-- | Chama a função atualizarTurma passando uma estrutura (estudante já editado) e a lista de estruturas
    let estudantesAtualizado = atualizarTurma novoEstudante estudantes

-- | Retorna uma lista de estruturas atualizada (lista com estudante editado)
    return estudantesAtualizado
        where 
           texto =  [ "========================================",
                      "     Editar Informações do Estudante    ",
                      "========================================",
                      "Opcoes:\n",
                      "1 - Alterar nome",
                      "2 - Alterar matrícula",
                      "3 - Alterar nota da prova 1",
                      "4 - Alterar nota da prova 2",
                      "5 - Alterar nota da prova 3",
                      "6 - Alterar nota do trabalho 1",
                      "7 - Alterar nota do trabalho 2",
                      "8 - Alterar número de faltas",
                      "9 - Voltar ao menu anterior" ]

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para editar um estudante. Recebe uma opção e uma estrutura (estudante a ser editado)
editaEstudante :: Int -> Estudantes -> IO Estudantes

-- | Em todas a opções cria-se uma nova estrutura com a informação editada desejada, e então retorna essa nova estrutura

editaEstudante opcao estudante | (opcao == 1) = do putStr "Digite o nome: "
                                                   novoNome <- getLine
                                                   let novoEstudante = estudante {nome = novoNome}
                                                   return novoEstudante

                               | (opcao == 2) = do putStr "Digite a matrícula: "
                                                   novaMatricula <- getLine
                                                   let novoEstudante = estudante {matricula = novaMatricula}
                                                   return novoEstudante

{-| 
   Da opção 3 para frente, é necessário recalcular a nota final e a situação do estudante. Para isso, cria-se uma outra nova estrutura com a nota final e a situação atualizadas

   Exemplo:	Estudante (original) -> novoEstudante (cópia com informação editada) -> novoEstudanteNotaFinal (cópia da cópia com a nota final atualizada

|-}

                               | (opcao == 3) = do putStr "Digite a nota da prova 1: "
                                                   novap1 <- readLn

                                                   let novoEstudante = estudante {p1 = novap1}

-- | Para o novoEstudanteNotaFinal, chama-se a função calculaNotaFinal' passando o novoEstudante
                                                   novoEstudanteNotaFinal <- calculaNotaFinal' novoEstudante

                                                   return novoEstudanteNotaFinal

                               | (opcao == 4) = do putStr "Digite a nota da prova 2: "
                                                   novap2 <- readLn

                                                   let novoEstudante = estudante {p2 = novap2}

                                                   novoEstudanteNotaFinal <- calculaNotaFinal' novoEstudante

                                                   return novoEstudanteNotaFinal

                               | (opcao == 5) = do putStr "Digite a nota da prova 3: "
                                                   novap3 <- readLn

                                                   let novoEstudante = estudante {p3 = novap3}

                                                   novoEstudanteNotaFinal <- calculaNotaFinal' novoEstudante

                                                   return novoEstudanteNotaFinal

                               | (opcao == 6) = do putStr "Digite a nota do trabalho 1: "
                                                   novat1 <- readLn

                                                   let novoEstudante = estudante {t1 = novat1}

                                                   novoEstudanteNotaFinal <- calculaNotaFinal' novoEstudante

                                                   return novoEstudanteNotaFinal

                               | (opcao == 7) = do putStr "Digite a nota do trabalho 2: "
                                                   novat2 <- readLn

                                                   let novoEstudante = estudante {t2 = novat2}

                                                   novoEstudanteNotaFinal <- calculaNotaFinal' novoEstudante

                                                   return novoEstudanteNotaFinal

                               | (opcao == 8) = do putStr "Digite o número de faltas: "
                                                   novafaltas <- readLn

                                                   let novoEstudante = estudante {faltas = novafaltas}

-- | Para o novoEstudanteFaltas, chama-se a função calculaSituacao para novoEstudante, passando as faltas e notafinal de novoEstudante
                                                   let novoEstudanteFaltas = novoEstudante {situacao = calculaSituacao (faltas novoEstudante) (notafinal novoEstudante)}

                                                   return novoEstudanteFaltas

-- | Retorna o mesma estrutura (estudante não foi editado)
                               | (opcao == 9) = do return estudante

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para "atualizar" a lista de estruturas. Recebe uma estrutura (estudante editado), uma lista de estruturas e devolve uma lista de estruturas editada
atualizarTurma :: Estudantes -> [Estudantes] -> [Estudantes]

-- | Caso Base: se a lista de estruturas estiver vazia, retorna uma lista vazia
atualizarTurma _ [] = []

{-|
    Se a lista de estruturas não estiver vazia, buscará na lista o estudante que possuí a mesma matrícula que o estudante atualizado
    Ao encontrar, adicionará o estudante atualizado na lista de estruturas editada no lugar do estudante desatualizado
    Enquanto não encontrar, adicionará os estudantes lidos na lista de estruturas editada
|-}
atualizarTurma estudanteAtualizado (estudante:resto) | (matricula estudante == matricula estudanteAtualizado) = estudanteAtualizado : resto
                                                     | otherwise = estudante : atualizarTurma estudanteAtualizado resto

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para recalcular a nota final e situação de um estudante editado. Recebe uma estrutura (estudante editado) e volta uma estrutura (estudante com nota final e situação editados)
calculaNotaFinal' :: Estudantes -> IO Estudantes
calculaNotaFinal' novoEstudante = do

-- | Calcula a nova nota final do estudante editado
    let novanotaFinal = calculaNotaFinal (p1 novoEstudante) (p2 novoEstudante) (p3 novoEstudante) (t1 novoEstudante) (t2 novoEstudante)

-- | novoEstudanteNotaFinal é uma cópia de novoEstudante tendo a nota final atualizada
    let novoEstudanteNotaFinal = novoEstudante {notafinal = novanotaFinal}

-- | novoEstudanteSituacao é uma cópia do novoEstudanteNotaFinal tendo a situação recalculada pela função CalculaSituação
    let novoEstudanteSituacao = novoEstudanteNotaFinal {situacao = calculaSituacao (faltas novoEstudanteNotaFinal) (notafinal novoEstudanteNotaFinal)}

-- | Retorna novoEstudanteSituacao
    return novoEstudanteSituacao

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função que irá reler um arquivo e restaurar a lista de estruturas para os valores lidos do arquivo. Recebe uma string (nome do arquivo) e devolve uma lista de estruturas
relerTurma :: String -> IO [Estudantes]
relerTurma arquivo = 

-- | Caso o arquivo não exista (é uma string vazia), imprime a mensagem de que o arquivo não existe e retorna a lista vazia
    if (arquivo == "") then do putStrLn ("\nArquivo " ++ arquivo ++ " não existe!")
                               return []

-- | Caso o arquivo exista, o arquivo será relido e a lista de estruturas restaurada
                       else do putStr ("\nArquivo " ++ arquivo ++ " lido com sucesso!")
                               novoEstudantes <- lerTurma arquivo
                               return novoEstudantes

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função que irá salvar todas a modificações feitas em um novo arquivo. Recebe uma string (nome do arquivo) e uma estrutura.
salvarArquivo :: String -> [Estudantes] -> IO ()
salvarArquivo arquivo estudantes = do

-- | Caso o arquivo não exista (é uma string vazia), imprime a mensagem de que o arquivo não existe e pergunta se deseja cria-lo
    if (arquivo == "") then do putStr ("O arquivo " ++ arquivo ++ " não existe. Deseja criar um novo? (1 - sim / 2 - não)?")
                               opcao <- lerOpcao''

-- | Se a resposta for sim, cria um arquivo com o nome passado e guarda no arquivo uma string com todo o conteúdo da lista de estruturas
                               if (opcao == 1) then do writeFile ("Novo" ++ arquivo) (estrutura2String estudantes)
                                                       encerra

-- | Se a resposta for não, apenas encerra o programa
                                               else encerra

-- | Caso o arquivo exista, criará um nova versão do arquivo com todas a modificações feitas e depois encerrará o programa
                       else do writeFile ("Novo" ++ arquivo) (estrutura2String estudantes)
                               encerra

{-| 
    RECADO: Por causa do erro "*** Exception: turma1.txt: withFile: resource busy (file is locked)" não consegui abrir o arquivo para colocar as modificações
    feitas pelo usuário. Com objetivo de tornar o programa funcional, fiz uma pequena mudança na função 7. Ao invés de substituir o arquivo, a função sempre
    criará um novo contendo as novas informações.
|-}

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função para colocar todo o conteúdo da lista de estruturas em uma string. Recebe uma lista de estruturas e devolve uma string
estrutura2String :: [Estudantes] -> String

-- | Caso Base: se a lista de estruturas estiver vazia, retorna uma string vazia
estrutura2String [] = ""

{-| 
    Se a lista não estiver vazia, concatena as informações da lista de estruturas em uma string, separando as informações com "\n", para garantir que cada
    informação esteja em uma linha no arquivo
|-}
estrutura2String (estudante:resto) = nome estudante ++ "\n" ++ matricula estudante ++ "\n" ++ show (p1 estudante) ++ "\n" ++ show (p2 estudante) ++ "\n" ++
    show (p3 estudante) ++ "\n" ++ show (t1 estudante) ++ "\n" ++ show (t2 estudante) ++ "\n" ++ show (faltas estudante) ++
    "\n" ++ show (notafinal estudante) ++ "\n" ++ [situacao estudante] ++ "\n" ++ estrutura2String resto

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Menu Principal, recebe um inteiro, uma string e uma lista de estruturas
menu :: Int -> String -> [Estudantes] -> IO ()
menu opcao arquivo estudantes = do 

-- | Função para exibir as opções do Menu
    exibirOpcoes texto 

-- | Função para ler a opção desejada pelo usuário. Passa um valor mínimo e máximo
    opcao <- lerOpcao 1 7

-- | Função para processar a opção desejada. Passa a opção, o nome do arquivo e a lista de estudantes
    processarOpcao opcao arquivo estudantes

        where 
           texto =  [ "========================================",
                      "             Menu Principal             ",
                      "========================================",
                      "Opcoes:\n",
                      "1 - Ler turma de estudantes do arquivo",
                      "2 - Imprimir turma de estudantes",
                      "3 - Imprimir estatísticas da turma",
                      "4 - Cadastrar novo estudante",
                      "5 - Editar informações de um estudante",
                      "6 - Reler turma de estudantes do arquivo",
                      "7 - Salvar e Sair" ]


----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Principal Laço do programa. É responsável por chamar as opções, armazenando um novo nome de arquivo ou uma nova lista de estudantes
processarOpcao :: Int -> String -> [Estudantes] -> IO ()
processarOpcao opcao arquivo estudantes

--  | Recebe um novo nome de arquivo e uma nova lista de estudantes e os passa para o menu
    | (opcao == 1) = do novoarquivo <- menuLerTurma arquivo

--  | Chama a função lerTurma passando o novo nome de arquivo e guarda o retorno (uma lista de estruturas) em estudantes
                        estudantes <- lerTurma novoarquivo
                        menu 0 novoarquivo estudantes

--  | Chama a função imprimirTurma passando o arquivo e a lista de estudantes já armazenados
    | (opcao == 2) = do imprimirTurma arquivo estudantes
                        menu 0 arquivo estudantes

--  | Chama a função imprimirEstatistica passando o arquivo e a lista de estudantes já armazenados
    | (opcao == 3) = do imprimirEstatistica arquivo estudantes
                        menu 0 arquivo estudantes

--  | Recebe uma nova lista de estudantes, agora com um novo estudante cadastrado, e então passa para o menu
    | (opcao == 4) = do novoEstudantes <- cadastrarEstudante arquivo estudantes
                        menu 0 arquivo novoEstudantes

--  | Recebe uma nova lista de estudantes, agora com um novo estudante editado, e então passa para o menu
    | (opcao == 5) = do novoEstudantes <- menuEncontraEstudante arquivo estudantes
                        menu 0 arquivo novoEstudantes

--  | Restaura a lista de estudantes com as informações do arquivo, e então passa para o menu a nova lista
    | (opcao == 6) = do novoEstudantes <- relerTurma arquivo
                        menu 0 arquivo novoEstudantes

--  | Cria um novo arquivo contendo todas as alterações feitas e então finaliza o programa
    | (opcao == 7) = do salvarArquivo arquivo estudantes

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Ler e validar uma opcao com valor inteiro
lerOpcao :: Int -> Int -> IO Int
lerOpcao min max = do putStr "\nDigite uma opção: "
                      opcao <- readLn
                      valida opcao
    where
      valida opcao | (min <= opcao && opcao <= max) = return opcao
                   | otherwise = do putStrLn "\nOpcao invalida!\n"
                                    lerOpcao min max

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
lerOpcao' :: IO Int
lerOpcao' = do opcao <- getChar
               valida' opcao 
    where
      valida' opcao | (opcao == 's') = return 1
                    | (opcao == 'n') = return 2
                    | otherwise = do putStrLn "\nOpcao Invalida!\n"
                                     lerOpcao'

lerOpcao'' :: IO Int
lerOpcao'' = do opcao <- readLn
                valida'' opcao
    where
      valida'' opcao | (opcao == 1) || (opcao == 2) = return opcao
                     | otherwise = do putStrLn "\nOpcao Invalida!\n"
                                      lerOpcao''

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
main :: IO ()
main = do hSetBuffering stdin NoBuffering

{-| 
   A Main chama a função main passando 0 (campo que armazena a opcao), uma string vazia (campo que armazena o nome do arquivo) 
   e uma lista vazia (campo que armazena a lista de estruturas) 
|-}
          menu 0 "" []

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Encerra o programa
encerra :: IO ()
encerra = do putStrLn "\n\nAté a próxima!\n\n"

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função que printa um texto qualquer (recebe uma lista de strings)
exibirOpcoes :: [String] -> IO ()
exibirOpcoes texto = do

    putStrLn ""
    putStrLn (unlines texto)
    putStrLn ""

----------------------------------------------------------------------------------------------------------------------------------------------------------------------      
-- | Função que, dado um valor inteiro, devolve uma string de espaços
makespaces :: Int -> String
makespaces 0 = []
makespaces n = ' ' : makespaces (n - 1) 