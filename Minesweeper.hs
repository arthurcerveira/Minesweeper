module Main where

-- Aqui estou importando algumas funções para transformar de inteiros para caracteres
--  e vice-vesa, funções de entrada/saída e números aleatóreos:

import Data.Char
import System.IO
import System.Random

-- Tabuleiro do jogo:
type GBoard = [[Char]]
-- Tabuleiro que contem a posicao das minas (Mapa de Minas). True = mina, False = sem mina:
type MBoard = [[Bool]]

-- Exemplo de Tabuleiro 9x9 inicial todo fechado:
gBoard :: GBoard
gBoard = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]

-- Exemplo de tabuleiro 9x9 com a posição das minas:

mBoard :: MBoard
mBoard = [[True, False, False, False, False, False, False, False, True],
          [False, False, False, False, False, False, False, False, False],
          [False, True, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, True , False, False, False, False],
          [False, False, False, False, False, True, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [True, False, False, False, False, False, False, False, True]]

-- PRIMEIRA PARTE - FUNÇÕES PARA MANIPULAR OS TABULEIROS DO JOGO (MATRIZES)

-- A ideia das próximas funções é permitir que a gente acesse uma lista usando um indice,
-- como se fosse um vetor

-- gArr (get array): recebe uma posicao (p) e uma lista (vetor) e devolve o elemento
-- na posição p do vetor

gArr :: Int -> [t] -> t
gArr n l = l !! n

-- uArr (update array): recebe uma posição (p), um novo valor (v), e uma lista (vetor) e devolve um
-- novo vetor com o valor v na posição p 

uArr :: Int -> a -> [a] -> [a]
uArr n a (l:ls)
   | n > 0 = l : uArr (n - 1) a ls
   | otherwise = a : ls

-- Uma matriz, nada mais é do que um vetor de vetores. 
-- Dessa forma, usando as operações anteriores, podemos criar funções para acessar os tabuleiros, como 
-- se  fossem matrizes:

-- gPos (get position) recebe linha (l), coluna (c) (não precisa validar) e um tabuleiro. Devolve o elemento na posicao
-- tabuleiro[l,c]. Usar gArr na implementação

gPos :: Int -> Int -> [[a]] -> a
gPos l c m = gArr c (gArr l m)

-- uPos (update position): recebe um novo valor, uma posição no tabuleiro (linha e coluna) e um tabuleiro. Devolve 
-- o tabuleiro modificado com o novo valor na posiçao lxc

uPos :: Int -> Int -> a -> [[a]] -> [[a]]
uPos l c a m = uArr l (uArr c a (gArr l m)) m

--------------- SEGUNDA PARTE: LÓGICA DO JOGO

-- isMine: recebe linha coluna e o tabuleiro de minas, e diz se a posição contém uma mina

isMine :: Int -> Int -> MBoard -> Bool
isMine l c m = gPos l c m

-- isValidPos: recebe o tamanho do tabuleiro (ex, em um tabuleiro 9x9, o tamanho é 9), 
-- uma linha e uma coluna, e diz se essa posição é válida no tabuleiro

isValidPos :: Int -> Int -> Int -> Bool
isValidPos t l c = (l < t) && (c < t) && (l >= 0) && (c >= 0)

-- 
-- validMoves: Dado o tamanho do tabuleiro e uma posição atual (linha e coluna), retorna uma lista
-- com todas as posições adjacentes à posição atual

-- Exemplo: Dada a posição linha 3, coluna 3, as posições adjacentes são: [(2,2),(2,3),(2,4),(3,2),(3,4),(4,2),(4,3),(4,4)]
-- ...   ...      ...    ...   ...
-- ...  (2,2)    (2,3)  (2,4)  ...
-- ...  (3,2)    (3,3)  (3,4)  ...
-- ...  (4,2)    (4,3)  (4,4)  ...
-- ...   ...      ...    ...   ...

--  Dada a posição (0,0) que é um canto, as posições adjacentes são: [(0,1),(1,0),(1,1)]

--  (0,0)  (0,1) ...
--  (1,0)  (1,1) ...
--   ...    ...  ..

adjPos :: [(Int, Int)]
adjPos = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves t l c = getAdjPos adjPos t l c
  where 
     getAdjPos :: [(Int, Int)] -> Int -> Int -> Int -> [(Int,Int)]
     getAdjPos [] t l c = []
     getAdjPos (x:xs) t l c
       | isValidPos t ((fst x) + l) ((snd x) + c) = ((fst x) + l, (snd x) + c) : getAdjPos xs t l c
       | otherwise = getAdjPos xs t l c


-- cMinas: recebe uma posicao  (linha e coluna), o tabuleiro com o mapa das minas, e conta quantas minas
-- existem nas posições adjacentes

cMinas :: Int -> Int -> MBoard -> Int
cMinas l c m = adjMinas (validMoves (length m) l c) m
  where
     adjMinas :: [(Int,Int)] -> MBoard -> Int
     adjMinas [] m = 0
     adjMinas (x:xs) m
       | (gPos (fst x) (snd x) m) == True = 1 + adjMinas xs m
       | otherwise = adjMinas xs m

---
--- abreJogada: é a função principal do jogo!!
--- recebe uma posição a ser aberta (linha e coluna), o mapa de minas e o tabuleiro do jogo. Devolve como
--  resposta o tabuleiro do jogo modificado com essa jogada.
--- Essa função é recursiva, pois no caso da entrada ser uma posição sem minas adjacentes, o algoritmo deve
--- seguir abrindo todas as posições adjacentes até que se encontre posições adjacentes à minas.
--- Vamos analisar os casos:
--- - Se a posição a ser aberta é uma mina, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta já foi aberta, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta é adjacente a uma ou mais minas, devolver o tabuleiro modificado com o número de
--- minas adjacentes na posição aberta
--- - Se a posição a ser aberta não possui minas adjacentes, abrimos ela com zero (0) e recursivamente abrimos
--- as outras posições adjacentes a ela

abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada l c m g
  | (gPos l c m) = uPos l c '*' g -- Posicao ser uma mina
  | (gPos l c g) /= '-' = g -- Posicao estar aberta
  | (cMinas l c m) /= 0 = uPos l c (intToChar l c m) g -- Posicao com minas adjacentes
  | (cMinas l c m) == 0 = abreJogadaRecur (validMoves (length m) l c) l c m (uPos l c '0' g) -- Posicao sem minas adjacentes
    where
       abreJogadaRecur :: [(Int,Int)] -> Int -> Int -> MBoard -> GBoard -> GBoard
       abreJogadaRecur [] l c m g = g
       abreJogadaRecur (x:xs) l c m g = abreJogadaRecur xs l c m (abreJogada (fst x) (snd x) m g)
       
       intToChar :: Int -> Int -> MBoard -> Char
       intToChar l c m = (chr ((cMinas l c m) + 48))

--- abreTabuleiro: recebe o mapa de Minas e o tabuleiro do jogo, e abre todo o tabuleiro do jogo, mostrando
--- onde estão as minas e os números nas posições adjecentes às minas. Essa função é usada para mostrar
--- todo o tabuleiro no caso de vitória ou derrota

abreTabuleiro :: MBoard -> GBoard -> GBoard
abreTabuleiro m g = abreTabuleiroRecur 0 0 m g
  where
     abreTabuleiroRecur :: Int -> Int -> MBoard -> GBoard -> GBoard
     abreTabuleiroRecur l c m g
       | c == (length m) = g
       | l == (length m - 1) = abreTabuleiroRecur 0 (c + 1) m (abreJogada l c m g)
       | otherwise = abreTabuleiroRecur (l + 1) c m (abreJogada l c m g)

--  -- contaFechadas: Recebe um GBoard e conta quantas posições fechadas existem no tabuleiro (posições com '-')

contaFechadas :: GBoard -> Int
contaFechadas [] = 0
contaFechadas (g:gs) = contaFechadaRecur g + contaFechadas gs
  where
   contaFechadaRecur :: [Char] -> Int
   contaFechadaRecur [] = 0
   contaFechadaRecur (c:cs)
     | c == '-' = 1 + contaFechadaRecur cs
     | otherwise = contaFechadaRecur cs

-- contaMinas: Recebe o tabuleiro de Minas (MBoard) e conta quantas minas existem no jogo

contaMinas :: MBoard -> Int
contaMinas [] = 0
contaMinas (m:ms) = contaMinasRecur m + contaMinas ms
  where
   contaMinasRecur :: [Bool] -> Int
   contaMinasRecur [] = 0
   contaMinasRecur (c:cs)
     | c = 1 + contaMinasRecur cs
     | otherwise = contaMinasRecur cs

-- endGame: recebe o tabuleiro de minas, o tabuleiro do jogo, e diz se o jogo acabou.
-- O jogo acabou quando o número de casas fechadas é igual ao numero de minas

endGame :: MBoard -> GBoard -> Bool
endGame m g = contaMinas m == contaFechadas g

---
---  PARTE 3: FUNÇÕES PARA GERAR TABULEIROS E IMPRIMIR TABULEIROS
---

-- printBoard: Recebe o tabuleiro do jogo e devolve uma string que é a representação visual desse tabuleiro
-- Usar como referncia de implementacao o video sobre tabela de vendas (Aula 06)

printBoard :: GBoard -> String
printBoard [] = ""
printBoard (g:gs) = "| " ++ (printBoardLinha g) ++ "|\n" ++ (printBoard gs)
  where
    printBoardLinha :: [Char] -> String
    printBoardLinha [] = ""
    printBoardLinha (l:ls) = l : " " ++ printBoardLinha ls

-- Imprime mapa de minas para debug
printMBoard :: MBoard -> String
printMBoard [] = ""
printMBoard (g:gs) = (printMBoardLinha g) ++ "\n" ++ (printMBoard gs)
  where
    printMBoardLinha :: [Bool] -> String
    printMBoardLinha [] = ""
    printMBoardLinha (l:ls)
      | (l) = "True\t" ++ printMBoardLinha ls
      | otherwise = "False\t" ++ printMBoardLinha ls

-- geraLista: recebe um inteiro n, um valor v, e gera uma lista contendo n vezes o valor v

geraLista :: Int -> a -> [a]
geraLista n v
  | n > 0 = v : geraLista (n - 1) v
  | otherwise = []

-- geraTabuleiro: recebe o tamanho do tabuleiro e gera um tabuleiro  novo, todo fechado (todas as posições
-- contém '-'). A função geraLista deve ser usada na implementação

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro n = geraNovoTabuleiroAux n n
  where
    geraNovoTabuleiroAux n m
      | n > 0 = geraLista m '-' : geraNovoTabuleiroAux (n - 1) m
      | otherwise = []

-- geraMapaDeMinasZerado: recebe o tamanho do tabuleiro e gera um mapa de minas zerado, com todas as posições
-- contendo False. Usar geraLista na implementação

geraMapaDeMinasZerado :: Int -> MBoard
geraMapaDeMinasZerado n = geraMapaDeMinasZeradoAux n n
  where
    geraMapaDeMinasZeradoAux n m
      | n > 0 = geraLista m False : geraMapaDeMinasZeradoAux (n - 1) m
      | otherwise = []

-- A função a seguir (main) deve ser substituida pela função main comentada mais
-- abaixo quando o jogo estiver pronto

-- main :: IO ()
-- main = print "Alo Mundo!"

-- {-

-- Aqui está o Motor do Jogo.
-- Essa parte deve ser descomentada quando as outras funções estiverem implementadas
-- Para rodar o jogo, digite "main" no interpretador

main :: IO ()
main = do
   putStr "Digite o tamanho do tabuleiro: "
   size <- getLine
   mb <- genMinesBoard (read size)
   gameLoop mb (geraNovoTabuleiro (read size)) 

gameLoop :: MBoard -> GBoard -> IO ()
gameLoop mb gb = do
   putStr (printBoard gb)
  --  putStr (printMBoard mb)
   putStr "Digite uma linha: "
   linha <- getLine
   putStr "Digite uma coluna: "
   coluna <- getLine
   if (isMine (read linha) (read coluna) mb)
      then do
            putStr "VOCE PERDEU!\n"
            putStr $ printBoard $ abreTabuleiro mb gb
            putStr "TENTE NOVAMENTE!\n"
      else do
            let newGB = (abreJogada (read linha) (read coluna) mb gb)
            if (endGame mb newGB)
                 then do
                     putStr "VOCE VENCEU!!!!!!!!\n"
                     putStr $ printBoard $ abreTabuleiro mb newGB
                     putStr "PARABENS!!!!!!!!!!!\n"
                 else
                     gameLoop mb newGB




----- DO NOT GO BEYOUND THIS POINT   


genMinesBoard :: Int -> IO MBoard
genMinesBoard size = do
        board <- addMines (round   ((fromIntegral (size *size)) * 0.15)) size (geraMapaDeMinasZerado size) 
        return board

addMines :: Int -> Int -> MBoard -> IO MBoard
addMines 0 size b = return b
addMines n size b = do
                l <- randomRIO (0,(size-1))
                c <- randomRIO (0,(size-1))
                case isMine l c b of
                      True -> addMines n size b
                      False -> addMines (n-1) size (uPos l c True b)
