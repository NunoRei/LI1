{-|
  = Tarefa 2
-}
module Main where

import Data.Char (isDigit)
import System.Environment
import Data.List

{-|
  Função que dado um estado de jogo, um número de jogador, e um comando, altera este input
  para uma forma mais conveniente e envia o resultado para outras funções, acabando por
  devolver um novo estado de jogo correspondente à reação ao comando.

    == Utilização:
    
    >>> move 0 'L' ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","! 5 5","* 7 7 1 1 10","0 3 3 ++","1 7 7"]
-}

move :: [String] -> Int -> Char -> [String]
move [] _ _ = [] 
move estado p c | c == 'U' = moveTwo estado (show p) c
                | c == 'D' = moveTwo estado (show p) c
                | c == 'L' = moveTwo estado (show p) c 
                | c == 'R' = moveTwo estado (show p) c
                | c == 'B' && existeplayer estado p = if cPowerUpcBombasMapa (nBombas (infop estado p)) (bombasPlayerMapa (bombamapa estado) p 0) && ntemBomba (bombamapa estado) (infop estado p)  then dividemapa estado ++ ordenaBombas (bombamapa estado) (novaBomba (infop estado p) p) ++ listadosp estado else estado
                | otherwise = estado

{-|
  Função dada previamente para testar o funcionamento da função move.
-}

main :: IO () 
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
{-|
  Função que recebe o estado de jogo e o inteiro referente ao jogador, e devolve a String que
  contém a informação sobre esse jogador, o seu número, as suas coordenadas e os powerups que
  possui.

    == Utilização:
    
    >>> infop ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 0  
    "0 4 3 +"
-}

infop :: [String] -> Int -> String
infop [] _ = "" 
infop (x:xs) p = if (head x == head (show p)) then x else infop xs p

{-|
  Função que recebe a String com as coordenadas e os powerups do jogador e devolve a
  String com a coluna em que se encontra.
  
    == Utilização:
    
    >>> colunap "4 3 +"  
    "4"
-}

colunap :: String -> String
colunap [] = ""                
colunap (x:xs) | x /= ' ' = x : colunap xs
               | otherwise = ""

{-|
  Função que recebe a String com as coordenadas e os powerups do jogador e devolve a
  String com a linha em que se encontra.
  
    == Utilização:
    
    >>> linhap "4 3 +"  
    "3"
-}  

linhap :: String -> String 
linhap str = colunap ((\\) str (colunap str ++ " "))

{-|
  Função que recebe o estado de jogo e devolve a lista de Strings com todas as bombas colocadas
  no mapa.

    == Utilização:
    
    >>> bombamapa ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]  
    ["* 7 7 1 1 10"]
-}
 
bombamapa :: [String] -> [String]
bombamapa [] = []
bombamapa (x:xs) = if (head x == '*') then x : bombamapa xs else bombamapa xs

{-|
  Função que dada a lista de Strings com todas as bombas colocadas no mapa, o inteiro
  referente ao jogador, e um contador que vai começar em 0, devolve o número de bombas
  que esse jogador tem no mapa.

    == Utilização:
    
    >>> bombasPlayerMapa ["* 7 7 1 1 10","* 8 9 1 1 10"] 1 0
    2 
-} 

bombasPlayerMapa :: [String] -> Int -> Int -> Int 
bombasPlayerMapa [] _ n = n 
bombasPlayerMapa (x:xs) p n = if (verificaString x p 0) then bombasPlayerMapa xs p (n+1) else bombasPlayerMapa xs p n
                  where verificaString :: String -> Int -> Int -> Bool
                        verificaString (x:xs) p 3 = if (x == head (show p)) then True else False 
                        verificaString (x:xs) p n = if (x == ' ') then verificaString xs p (n+1) else verificaString xs p n

{-|
  Função que dado o número de bombas que um jogador porde colocar e as bombas que já
  colocou no mapa, compara esses números e se ainda poder pôr bomba devolve True.

    == Utilização:
    
    >>> cPowerUpcBombasMapa 1 1 
    False 
-}
                          
cPowerUpcBombasMapa :: Int -> Int -> Bool
cPowerUpcBombasMapa p b | p > b = True
                        | otherwise = False

{-|
  Função que dada a String com a informação acerca do jogador, devolve o número 
  de bombas que pode colocar.

    == Utilização:
    
    >>> nBombas "0 4 3 +"
    2      
-}                          

nBombas :: String -> Int
nBombas [] = 1
nBombas (x:xs) | x == '+' = 1 + nBombas xs
               | otherwise = nBombas xs

{-|
  Função que dada a String com a informação acerca do jogador, devolve o raio 
  de explosão das suas bombas.

    == Utilização:
    
    >>> nFlames "1 7 7 !"
    2      
-}                

nFlames :: String -> Int 
nFlames [] = 1
nFlames (x:xs) | x == '!' = 1 + nFlames xs
               | otherwise = nFlames xs

{-|
  Função que dada a lista de Strings com todas as bombas colocadas no mapa, e a String
  com a informação do jogador, vai comparar as coordenadas em que o jogador se encontra
  com as coordenadas de todas as bombas colocadas no mapa, e vai devolver True caso não
  exista bomba nas coordenadas do jogador.

    == Utilização:
    
    >>> ntemBomba ["* 7 7 1 1 10","* 8 9 1 1 10"] "1 8 9 +"
    False    
-}               

ntemBomba :: [String] -> String -> Bool
ntemBomba [] _ = True
ntemBomba (x:xs) str | colunap (drop 2 x) == colunap (drop 2 str) && linhap (drop 2 x) == linhap (drop 2 str) = False 
                     | otherwise = ntemBomba xs str

{-|
  Função que recebe a String com a informação do jogador, e o inteiro referente ao jogador
  e cria a String de uma nova bomba. 

    == Utilização:

    >>> novaBomba "1 8 9 +" 1
    "* 8 9 1 1 10"    
-}                     

novaBomba :: String -> Int -> String
novaBomba str p = "* " ++ colunap (drop 2 str) ++ " " ++ linhap (drop 2 str) ++ " " ++ show p ++ " " ++ show (nFlames str) ++ " " ++ "10"

{-|
  Função que recebe a lista de Strings com todas as bombas colocadas no mapa, e recebe a String
  da nova bomba a ser colocada, vai colocar esta nova String na lista ordenando-a conforme as 
  coordenadas.    

    == Utilização:

    >>> ordenaBombas ["* 7 7 1 1 10","* 8 9 1 1 10"] "* 6 5 0 2 10" 
    ["* 6 5 0 2 10","* 7 7 1 1 10","* 8 9 1 1 10"]    
-}     
 
ordenaBombas :: [String] -> String -> [String]
ordenaBombas [] nb = nb : []  
ordenaBombas (x:xs) nb | (read (linhap (drop 2 x)) :: Int) > (read (linhap (drop 2 nb)) :: Int) = nb : x : xs
                       | (read (linhap (drop 2 x)) :: Int) < (read (linhap (drop 2 nb)) :: Int) = x : ordenaBombas xs nb
                       | otherwise = if verificaColuna x nb then nb : x : xs else ordenaBombas xs nb
                             where verificaColuna :: String -> String -> Bool
                                   verificaColuna x nb | (read (colunap (drop 2 x)) :: Int) > (read (colunap (drop 2 nb)) :: Int) = True
                                                       | otherwise = False

{-|
  Função que recebe o estado de jogo, e devolve a lista de Strings com as Strings que têm a 
  informação de todos os jogadores.    

    == Utilização:

    >>> listadosp ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 
    ["0 4 3 +","1 7 7"]   
-}                                                       

listadosp :: [String] -> [String]
listadosp [] = []
listadosp (x:xs) = if isDigit (head x) then x : listadosp xs else listadosp xs

{-|
  Função que recebe o estado de jogo, e divide essa lista de Strings, removendo tudo o que
  aparece a seguir aos powerups.    

    == Utilização:

    >>> dividemapa ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5"]   
-}                        
  
dividemapa :: [String] -> [String]
dividemapa [] = []
dividemapa (x:xs) | isDigit (head x) = dividemapa xs
                  | head x == '*' = dividemapa xs 
                  | otherwise = x : dividemapa xs

{-|
  Função que recebe o estado de jogo, e o inteiro referente ao jogador, e devolve True ou 
  False caso o jogador exista ou não.    

    == Utilização:

    >>> existeplayer ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 3 
    False   
-}                  

existeplayer :: [String] -> Int -> Bool
existeplayer [] _ = False  
existeplayer (x:xs) p = if (head x == head (show p)) then True else existeplayer xs p                     

{-|
  Função que devolve o novo estado de jogo para o caso de ser possível o jogador mover-se
  para a esquerda.

    == Utilização:
    
    >>> left ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "0" "+" 
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 3 3 ++","1 7 7"]
-}

left::[String]-> String -> String -> [String]
left [] plr ch = []
left ((p:e:crd):tst) plr ch | p == head plr && ch /= "" = [p:e:(leftAndUp (tira crd)) ++ " " ++ (tira (tail (mantem crd))) ++ (addPowerUp crd ch)] ++ tst
                            | p == head plr = [p:e:(leftAndUp (tira crd)) ++ (mantem crd)] ++ tst
                            | otherwise = [(p:e:crd)] ++ left tst plr ch

{-|
  Função que devolve o novo estado de jogo para o caso de ser possível o jogador mover-se
  para a direita.

    == Utilização:
    
    >>> right ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "0" "" 
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 5 3 +","1 7 7"]
-}

right::[String]-> String -> String-> [String]
right [] plr ch = []
right ((p:e:crd):tst) plr ch | p == head plr && ch /= "" = [p:e:(rightAndDown (tira crd)) ++ " " ++ (tira (tail (mantem crd))) ++ (addPowerUp crd ch)] ++ tst
                             | p == head plr = [p:e:(rightAndDown (tira crd)) ++ mantem crd ++ ch] ++ tst
                             | otherwise = [(p:e:crd)] ++ right tst plr ch

{-|
  Função que devolve o novo estado de jogo para o caso de ser possível o jogador mover-se
  para cima.

    == Utilização:
    
    >>> up ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "1" "" 
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 6"]
-}

up::[String] -> String -> String -> [String]
up [] plr ch = []
up ((p:e:crd):tst) plr ch | p == head plr && ch /= "" = [p:e:(tira crd)++ " " ++ leftAndUp (tira (tail (mantem crd))) ++ (addPowerUp crd ch)] ++ tst
                          | p == head plr = [p:e:(tira crd)++ " " ++ leftAndUp (tira (tail (mantem crd))) ++ mantem (tail(mantem crd))] ++ tst
                          | otherwise = [(p:e:crd)] ++ up tst plr ch
{-|
  Função que devolve o novo estado de jogo para o caso de ser possível o jogador mover-se
  para baixo.

    == Utilização:
    
    >>> down ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 5"] "1" "" 
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 6"]
-}

down::[String ]-> String -> String -> [String]
down [] plr ch = []
down ((p:e:crd):tst) plr ch | p == head plr && ch /= "" = [p:e:(tira crd)++ " " ++ rightAndDown (tira (tail (mantem crd))) ++ (addPowerUp crd ch)] ++ tst
                            | p == head plr = [p:e:(tira crd)++ " " ++ rightAndDown (tira (tail (mantem crd))) ++ mantem (tail(mantem crd))] ++ tst
                            | otherwise = [(p:e:crd)] ++ down tst plr ch

{-|
  Função que dada uma string com a informação de um jogador e uma string com um identificador
  de um powerup, adiciona esse powerup à informação do jogador.
    
    == Utilização:
    
    >>> addPowerUp "11 14 ++!" "+"
    " +++!"
-}

addPowerUp:: String -> String -> String
addPowerUp crd "+" | last crd == '!' || last crd == '+' = " +" ++ (tira (tail (mantem (tail (mantem crd)))))
                   | otherwise = " +"

addPowerUp crd "!" | last crd == '!' || last crd == '+' = " " ++(tira (tail (mantem (tail (mantem crd))))) ++ "!"
                   | otherwise = " !"

{-|
  Função que dado o estado de jogo e duas coordenadas, retorna o powerup que se encontra nessa
  posição se este existir ou uma string vazia.
    
    == Utilização:
    
    >>> sePowerUp ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 3 3
    "+"
-}

sePowerUp:: [String] -> Int -> Int -> String
sePowerUp [] l c = ""
sePowerUp ((p:e:crd):ts) l c | p == '+'  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = "+"
                             | p == '!' && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = "!"
                             | otherwise = sePowerUp ts l c
{-|
  Função que dado o estado de jogo e duas coordenadas, retorna o mesmo estado sem o powerup que
  se encontra nas coordenadas dadas.
    
    == Utilização:
    
    >>> elimina ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 3 3
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
-}

elimina :: [String] -> Int -> Int  -> [String]
elimina ((p:e:crd):ts) l c | (p == '+' || p == '!')  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = ts
                           | otherwise = [(p:e:crd)]++ elimina ts l c

{-|
  Função que dada uma string com digitos adiciona 1 ao valor que representam e devolve uma 
  string com o novo valor.
    
    == Utilização:
    
    >>> rightAndDown "33"
    "34"
-}

rightAndDown :: String -> String
rightAndDown x = show ((read (x)::Int) + 1)

{-|
  Função que dada uma string com digitos subtrai 1 ao valor que representam e devolve uma
  string com o novo valor.
    
    == Utilização:
    
    >>> leftAndUp "33"
    "32"
-}

leftAndUp :: String -> String
leftAndUp x = show ((read (x)::Int) - 1)

{-|
  Função que dada uma string, percorre-a até encontrar um espaço e devolve a string a
  partir desse espaço.
    
    == Utilização:
    
    >>> mantem "33 21 ++!"
    " 21 ++!"
-}

mantem ::String -> String
mantem "" = ""
mantem (hs:ts) | hs == ' ' = (hs:ts)
               | otherwise = mantem ts

{-|
  Função que dada uma string, percorre-a até encontrar um espaço e devolve uma string
  com o que está antes desse espaço.
    
    == Utilização:
    
    >>> tira "33 21 ++!"
    "33"
-}

tira :: String -> String
tira "" = ""
tira (hs:ts) | hs /= ' ' = hs:tira ts
             | hs == ' ' = ""

{-|
  Função que dado um estado de jogo e uma string com o numero do jogador, devolve o
  inteiro correspondente à coluna do mapa em que este se encontra.
    
    == Utilização:
    
    >>> checkC ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "0"
    4
-}

checkC :: [String] -> String -> Int
checkC [] plr = 0
checkC ((p:e:crd):tst) plr | p == head plr = (read (tira crd) :: Int)
                           | otherwise = checkC tst plr

{-|
  Função que dado um estado de jogo e uma string com o numero do jogador, devolve o
  inteiro correspondente à linha do mapa em que este se encontra.
    
    == Utilização:
    
    >>> checkL ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "1"
    7
-}

checkL :: [String] -> String -> Int
checkL [] plr = 0
checkL ((p:e:crd):tst) plr | p == head plr = (read (tira (tail (mantem crd))) :: Int)
                           | otherwise = checkL tst plr

{-|
  Função que dado um estado de jogo, um contador e as coordenadas da posição para onde
  o jogador se quer mover, com o auxílio da runMapTwo devolve o carater que se encontra
  nessa posição do mapa.
    
    == Utilização:
    
    >>> runMap ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] 0 3 3
    ' '
-}

runMap :: [String] -> Int -> Int -> Int -> Char
runMap [] l lv cv = '#'
runMap (hs:ts) l lv cv | l==lv = runMapTwo hs l 0 lv cv
                       | otherwise = runMap ts (l+1) lv cv

{-|
  Função que dada uma string do estado de jogo, o inteiro da linha a que corresponde, um
  contador e as coordenadas da posição para onde o jogador se quer mover, devolve o
  carater que se encontra nessa posição do mapa, que corresponde a uma pedra fixa '#',
  um espaço ' ', ou uma pedra destrutível '?'.
    
    == Utilização:
    
    >>> runMapTwo "#     ? #" 3 0 3 3
    ' '
-}

runMapTwo :: String -> Int -> Int -> Int -> Int -> Char
runMapTwo [] l c lv cv = '#'
runMapTwo (h:t) l c lv cv | c == cv = h
                          | otherwise = runMapTwo t l (c+1) lv cv


{-|
  Função que dada uma string do estado de jogo, uma string com o numero do jogador e um
  comando de movimento, verifica se é possível realizar esse comando e se algum powerup
  é apanhado, e em caso afirmativo chama a respetiva função de movimento com as
  coordenadas para onde o jogador quer ir e o carater correspondente ao powerup que foi
  apanhado caso este exista. Se o comando não for executável, devolve o estado de jogo
  recebido inicialmente.
    
    == Utilização:
    
    >>> moveTwo ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"] "0" 'L'
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","! 5 5","* 7 7 1 1 10","0 3 3 ++","1 7 7"]
-}

moveTwo :: [String] -> String -> Char -> [String]
moveTwo str p 'U'   | (runMap str 0 ((checkL str p) -1) (checkC str p)) == '#' || (runMap str 0 ((checkL str p) -1) (checkC str p)) == '?' = str
                    | (runMap str 0 ((checkL str p) -1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) -1) (checkC str p)) == "+" = (up (elimina str ((checkL str p) -1) (checkC str p)) p ("+"))
                    | (runMap str 0 ((checkL str p) -1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) -1) (checkC str p)) == "!" = (up (elimina str ((checkL str p) -1) (checkC str p)) p ("!"))
                    | (runMap str 0 ((checkL str p) -1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) -1) (checkC str p)) == "" = (up str p (""))

moveTwo str p 'D'   | (runMap str 0 ((checkL str p) +1) (checkC str p)) == '#' || (runMap str 0 ((checkL str p) +1) (checkC str p)) == '?' = str
                    | (runMap str 0 ((checkL str p) +1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) +1) (checkC str p)) == "+" = (down (elimina str ((checkL str p) +1) (checkC str p)) p ("+"))
                    | (runMap str 0 ((checkL str p) +1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) +1) (checkC str p)) == "!" = (down (elimina str ((checkL str p) +1) (checkC str p)) p ("!"))
                    | (runMap str 0 ((checkL str p) +1) (checkC str p)) == ' ' && (sePowerUp str ((checkL str p) +1) (checkC str p)) == "" = (down str p (""))

moveTwo str p 'L'   | (runMap str 0 (checkL str p) ((checkC str p) -1)) == '#' || (runMap str 0 (checkL str p) ((checkC str p) -1)) == '?' = str
                    | (runMap str 0 (checkL str p) ((checkC str p) -1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) -1)) == "+" = (left (elimina str (checkL str p) ((checkC str p) -1)) p ("+"))
                    | (runMap str 0 (checkL str p) ((checkC str p) -1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) -1)) == "!" = (left (elimina str (checkL str p) ((checkC str p) -1)) p ("!"))
                    | (runMap str 0 (checkL str p) ((checkC str p) -1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) -1)) == "" = (left str p (""))

moveTwo str p 'R'   | (runMap str 0 (checkL str p) ((checkC str p) +1)) == '#' || (runMap str 0 (checkL str p) ((checkC str p) +1)) == '?' = str
                    | (runMap str 0 (checkL str p) ((checkC str p) +1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) +1)) == "+" = (right (elimina str (checkL str p) ((checkC str p) +1)) p ("+"))
                    | (runMap str 0 (checkL str p) ((checkC str p) +1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) +1)) == "!" = (right (elimina str (checkL str p) ((checkC str p) +1)) p ("!"))
                    | (runMap str 0 (checkL str p) ((checkC str p) +1)) == ' ' && (sePowerUp str (checkL str p) ((checkC str p) +1)) == "" = (right str p (""))
