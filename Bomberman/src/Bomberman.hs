{-|
  = Tarefa 1
-}

module Bomberman where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random 
import Data.Char (isDigit)
import Data.List

{-|
  Função que cria uma string de '?'s e ' 's em função da dimensão do mapa e de uma semente
  com o auxílio da fazrandom.
  
    == Utilização:

    >>> fazrandomtostring 7 0
    "   ??  ? "
-}

fazrandomtostring::Int -> Int -> String
fazrandomtostring a b = map (\ x -> if (x >= 0) && (x <= 39) then '?' else ' ') (fazrandom a b)

{-|
  Função que cria uma lista de inteiros em função da dimensão do mapa e de uma semente.

    == Utilização:
    
    >>> fazrandom  7 0
    [83,93,63,38,0,87,81,1,61]
-}

fazrandom :: Int -> Int -> [Int]
fazrandom a b = take (ncasas a) $ randomRs (0,99) (mkStdGen b)

{-|
  Função que calcula o número de casas do mapa que serão preenchidas por elementos da
  string gerada pela fazrandomtostring.

    == Utilização:
    
    >>> ncasas 7
    9
-}

ncasas :: Int -> Int
ncasas a | a == 5    = 0  
         | otherwise = (a * a) - ((2 * a) + (2 * (a - 2)) + ((contador a) * (contador a)) + 12) 

{-|
  Função que auxilia a ncasas a contar quantas casas têm de ser preenchidas ao contar
  o numero de ímpares entre cinco e a dimensão do mapa.

    == Utilização:

    >>> contador 9
    3
-}

contador :: Int -> Int 
contador a = if a == 5 then 1 else (1 + contador(a - 2))

{-|
  Função que recebe uma lista de inteiros da fazrandom e cria uma string com ' 's,
  '?'s, '!'s, e '+'s para auxiliar a função powerUps a descobrir as casas onde se
  encontram os powerups.

    == Utilização:

    >>> randToStr [83,93,63,38,0,87,81,1,61]
    "   ?+  + "
-}

randToStr:: [Int] -> String
randToStr [] = []
randToStr (hs:ts) | hs == 0 || hs == 1 = '+' : randToStr ts
          | hs == 2 || hs == 3 = '!' : randToStr ts
          | hs >= 4 && hs <= 39 = '?' : randToStr ts
          | otherwise = ' ': randToStr ts
 
{-|
  Função que dados dois inteiros, uma dimensão e uma semente, devolve uma lista de
  strings que representam o mapa e a localização dos powerups no mesmo.

    == Utilização:

    >>> mapa 7 0
    ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######","+ 3 3","+ 3 4"]
-}

mapa:: Int -> Int -> [String]
mapa 5 _ = replicate 5 '#' : "#   #" : "# # #" : "#   #" : [replicate 5 '#']
mapa x s = (pedraEspaco x x (fazrandomtostring x s))++(powerUps (pedraEspaco x x (randToStr (fazrandom x s))))

{-|
  Função que cria as casas predefinidas, ou seja, as paredes, os espaços nos cantos
  do mapa e as pedras no interior, e as junta com uma string de modo a obter uma lista
  de strings que correspondem ao mapa em função da sua dimensão.

    == Utilização:

    >>> pedraEspaco 7 7 "   ??  ? "
    ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"]
-}

pedraEspaco :: Int -> Int -> String -> [String]
pedraEspaco x k (hs:ts) | x==k = replicate x '#' : pedraEspaco  x (k-1) (hs:ts)
            | (x-1)==k = ("#  " ++ (take (x-6) (hs:ts)) ++ "  #") : pedraEspaco x (k-1) (drop (x-6) (hs:ts))
            | (x-2)==k = ("# " ++ (intercala (x-4) (hs:ts))  ++ " #") : pedraEspaco x (k-1) (drop (div (x-5) 2) (hs:ts))
            | k==3 = ("# " ++ (intercala (x-4) (hs:ts)) ++ " #") : pedraEspaco x (k-1) (drop (div (x-5) 2) (hs:ts))
            | k==2 = ("#  " ++ (take (x-6) (hs:ts))  ++ "  #") : pedraEspaco x (k-1) (hs:ts)
            | k==1 = replicate x '#':[]
            | otherwise = if even k
                      then ["#" ++ (take (x-2) (hs:ts)) ++ "#"] ++ (pedraEspaco x (k-1) (drop (x-2) (hs:ts)))
                      else intercala x (hs:ts) : (pedraEspaco x (k-1) (drop (div (x-1) 2) (hs:ts)))

{-|
  Função que nas linhas ímpares intercala uma pedra e um elemento duma string em
  função da dimensão do mapa.

    == Utilização:

    >>> intercala 7 "?? ??  ? "
    "#?#?# #"
-}

intercala :: Int -> String -> String
intercala 1 l = ['#']
intercala x (hl:tl)= '#':hl:intercala (x-2) tl

{-|
  Função que dada uma lista de strings devolve uma lista de strings que corresponde
  ao tipo e posição de cada powerup do mapa.

    == Utilização:

    >>> powerUps ["#######","#     #","# # # #","# ?+  #","# #+# #","#     #","#######"]
    ["+ 3 3","+ 3 4"]
-}

powerUps :: [String] -> [String]
powerUps [] = []
powerUps str = (bombs 0 str)++(flames 0 str)

{-|
  Função que procura as strings com bombs, e com o auxílio da bombsTwo devolve
  uma lista de strings em que cada string corresponde à localização de uma bomb.

    == Utilização:

    >>> bombs 0 ["#######","#     #","# # # #","# ?+  #","# #+# #","#     #","#######"]
    ["+ 3 3","+ 3 4"]
-}

bombs :: Int -> [String] -> [String]
bombs l [] = []
bombs l (hs:ts)= if elem ('+') hs then (bombsTwo l 0 hs)++ (bombs (l+1) ts)
                  else bombs (l+1) ts

{-|
  Função que dada a linha em que se encontram as bombs, calcula as suas localizações
  exatas e devolve uma lista de strings com elas.

    == Utilização:

    >>> bombsTwo 3 0 "# ?+  #"
    ["+ 3 3"]
-}

bombsTwo :: Int -> Int -> String ->  [String]
bombsTwo l c [] = []
bombsTwo l c (hs:ts) | hs == '+' = ("+ " ++ (show c) ++ " " ++ (show l)): bombsTwo l (c+1) ts
           | otherwise = bombsTwo l (c+1) ts

{-|
  Função que procura as strings com flames, e com o auxílio da flamesTwo devolve
  uma lista de strings em que cada string corresponde à localização de uma flame.

    == Utilização:

    >>> flames 0 ["#######","#     #","# # # #","# ?!  #","# #!# #","#     #","#######"]
    ["! 3 3","! 3 4"]
-}

flames :: Int -> [String] -> [String]
flames l [] = []
flames l (hs:ts)= if elem ('!') hs then (flamesTwo l 0 hs) ++ (flames (l+1) ts)
                   else flames (l+1) ts

{-|
  Função que dada a linha em que se encontram as flames, calcula as suas localizações
  exatas e devolve uma lista de strings com elas.

    == Utilização:

    >>> flamesTwo 3 0 "# ?!  #"
    ["! 3 3"]
-}

flamesTwo :: Int -> Int -> String ->  [String]
flamesTwo l c [] = []
flamesTwo l c (hs:ts) | hs == '!' = ("! " ++ (show c) ++ " " ++ (show l)): flamesTwo l (c+1) ts
            | otherwise = flamesTwo l (c+1) ts
{-|
 Função dada previamente que testa o funcionamento da mapa.
-}

{-|
  = Tarefa 2
-}


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
elimina [] _ _ = []
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
moveTwo str p _ = undefined

avanca :: [String] -> Int -> [String]
avanca (x:xs) t | t <= ((length x)-2)^2 = checksplosion (criaCrdR (x:xs) 0 ((length x)-2) 1 1 ((((length x)-2)^2)-t+1)) (criaCrdR (x:xs) 0 ((length x)-2) 1 1 ((((length x)-2)^2)-t+1)) 
        | otherwise = checksplosion (x:xs) (x:xs)


----
checksplosion :: [String] -> [String] ->[String]
checksplosion [] lst = lst
checksplosion ((c:e:crd):ts) lst | c == '*' && last crd == '1' = checksplosion ts (explode lst crd)
                 | c == '*' && last crd /= '1' = checksplosion ts (timeDown lst crd)
                 | otherwise = checksplosion ts lst

--         estado      str coord 
explode :: [String] -> String -> [String]
explode lst crd = explodeU (defuse lst crd) (read (tira (tail (mantem crd)))::Int) (read (tira crd)::Int) 0 (raio (mantem (tail (mantem crd))))

--           estado      linha  coluna kappa  raio
explodeU :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeU [] _ _ _ _ = []
explodeU lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = explodeR (killMap lst 0 l c) (l-k) c 0 r
           | k < r && (checkSpot lst l c) == '*' = explodeU (removePlayer (timeOne lst l c) l c) (l+1) c (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeU (removePlayer lst l c) (l+1) c (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeR (elimina1 lst l c) (l-k) c 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeU lst (l+1) c (k+1) r
           | k == r = explodeR (removePlayer (timeOne lst l c) l c) (l-k) c 0 r

explodeR :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeR [] _ _ _ _ = []
explodeR lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = explodeD (killMap lst 0 l c) l (c-k) 0 r
           | k < r && (checkSpot lst l c) == '*' = explodeR (removePlayer (timeOne lst l c) l c) l (c+1) (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeR (removePlayer lst l c) l (c+1) (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeD (elimina1 lst l c) l (c-k) 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeR lst l (c+1) (k+1) r
           | k == r = explodeD (removePlayer (timeOne lst l c) l c) l (c-k) 0 r

explodeD :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeD [] _ _ _ _ = []
explodeD lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = explodeL (killMap lst 0 l c) (l+k) c 0 r
           | k < r && (checkSpot lst l c) == '*' = explodeD (removePlayer (timeOne lst l c) l c) (l-1) c (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeD (removePlayer lst l c) (l-1) c (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeL (elimina1 lst l c) (l+k) c 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeD lst (l-1) c (k+1) r
           | k == r = explodeL (removePlayer (timeOne lst l c) l c) (l+k) c 0 r

explodeL :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeL [] _ _ _ _ = []
explodeL lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = killMap lst 0 l c
           | k < r && (checkSpot lst l c) == '*' = explodeL (removePlayer (timeOne lst l c) l c) l (c-1) (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeL (removePlayer lst l c) l (c-1) (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = (elimina1 lst l c)
           | k < r && (checkSpot lst l c) == 'n' = explodeL lst l (c-1) (k+1) r
           | k == r = removePlayer (timeOne lst l c) l c

checkSpot:: [String] -> Int -> Int -> Char
checkSpot [] l c = 'n'
checkSpot ((p:e:crd):ts) l c | p == '+'  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = p
                 | p == '!' && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = p
                 | p == '*' && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = p
                 | isDigit p && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = p
                 | otherwise = checkSpot ts l c

checkMap :: [String] -> Int -> Int -> Int -> Char
checkMap [] l lv cv = '#'
checkMap (hs:ts) l lv cv | l==lv = checkMapTwo hs l 0 lv cv
                       | otherwise = checkMap ts (l+1) lv cv


checkMapTwo :: String -> Int -> Int -> Int -> Int -> Char
checkMapTwo [] l c lv cv = '#'
checkMapTwo (h:t) l c lv cv | c == cv = h
                          | otherwise = checkMapTwo t l (c+1) lv cv


--      crd bomba
raio :: String -> Int
raio crdx = read (tira (tail (mantem (tail (mantem crdx)))))::Int

--          lst        linha  linhav colunav
killMap :: [String] -> Int -> Int -> Int -> [String]
killMap [] l lv cv = []
killMap (hs:ts) l lv cv | l==lv = ((killMapTwo hs l 0 lv cv):ts)
            | otherwise = (hs:(killMap ts (l+1) lv cv))

killMapTwo :: String -> Int -> Int -> Int -> Int -> String
killMapTwo [] l c lv cv = []
killMapTwo (h:t) l c lv cv | c == cv && h == '?' = (' ':t)
               | otherwise = (h:killMapTwo t l (c+1) lv cv)


removePlayer :: [String] -> Int -> Int  -> [String]
removePlayer [] _ _ = []
removePlayer ((p:e:crd):ts) l c | (p == '0' || p == '1' || p == '2' || p == '3')  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = removePlayer ts l c
                | otherwise = [(p:e:crd)] ++ removePlayer ts l c

defuse :: [String] -> String -> [String]
defuse [] _ = []
defuse ((p:e:crdx):ts) crd | p == '*' && (tira (tail (mantem crdx))) == (tira (tail (mantem crd))) && (tira crdx) == (tira crd) = defuse ts crd
               | otherwise = [(p:e:crdx)] ++ defuse ts crd

timeDown :: [String] -> String -> [String]
timeDown [] crd = []
timeDown ((p:e:crdx):t) crd | p == '*' && crdx == crd = [timeDownTwo (p:e:crdx)] ++ t
              | otherwise = [(p:e:crdx)]++ timeDown t crd

timeDownTwo :: String -> String
timeDownTwo str | last str == '0' = (take ((length str)-2) str) ++ "9"
        | last str <= '9' && last str >= '2' = (take ((length str)-1) str) ++ (show ((read (drop ((length str) - 1) str):: Int) - 1))
        | otherwise = str

timeOne :: [String] -> Int -> Int -> [String]
timeOne [] l c = []
timeOne ((p:e:crd):ts) l c | p /= '*' = [(p:e:crd)]++ timeOne ts l c
               | (tira (tail (mantem crd))) == (show l) && (tira crd) == show c && last crd == '0' = [(p:e:((take ((length crd) -2) crd) ++ "1"))]++ts
               | (tira (tail (mantem crd))) == (show l) && (tira crd) == show c && last crd >= '2' && last crd <= '9' = [(p:e:((take ((length crd) -1) crd) ++ "1"))]++ts
               | otherwise = [(p:e:crd)] ++ timeOne ts l c


--          estado      kappa  lenghtF linha  coluna  count
criaCrdR :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdR lst _ _ _ _ 0 = lst
criaCrdR lst k lng l c cnt | c /= (lng-k) = (criaCrdR (elimina1 (runMap1 lst 0 l c) l c ) lng k l (c+1) (cnt-1))
               | c == (lng-k) = (criaCrdD (elimina1 (runMap1 lst 0 l c) l c ) lng (k) (l+1) (c) (cnt-1))


criaCrdD :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdD lst _ _ _ _ 0 = lst
criaCrdD lst k lng l c cnt | l /= c = (criaCrdD (elimina1 (runMap1 lst 0 l c) l c ) lng k (l+1) (c) (cnt-1))
               | l == c = (criaCrdL (elimina1 (runMap1 lst 0 l c) l c ) lng k (l) (c-1) (cnt-1))

criaCrdL :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdL lst _ _ _ _ 0 = lst
criaCrdL lst k lng l c cnt | c /= (k+1) = (criaCrdL (elimina1 (runMap1 lst 0 l c) l c ) lng k (l) (c-1) (cnt-1))
               | c == (k+1) = (criaCrdU (elimina1 (runMap1 lst 0 l c) l c ) lng (k+1) (l-1) (c) (cnt-1))


criaCrdU :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdU lst _ _ _ _ 0 = lst
criaCrdU lst k lng l c cnt | l /= (c+1) = (criaCrdU (elimina1 (runMap1 lst 0 l c) l c ) lng k (l-1) (c) (cnt-1))
               | l == (c+1) = (criaCrdR (elimina1 (runMap1 lst 0 l c) l c ) lng k (l) (c+1) (cnt-1))

--         estado    linha  linhaV colunaV
runMap1 :: [String] -> Int -> Int -> Int -> [String]
runMap1 [] l lv cv = []
runMap1 (hs:ts) l lv cv | l==lv = ((runMapTwo1 hs l 0 lv cv):ts)
             | otherwise = (hs:(runMap1 ts (l+1) lv cv))

runMapTwo1 :: String -> Int -> Int -> Int -> Int -> String
runMapTwo1 [] l c lv cv = []
runMapTwo1 (h:t) l c lv cv | c == cv = ('#':t)
              | otherwise = (h:runMapTwo1 t l (c+1) lv cv)


elimina1 :: [String] -> Int -> Int  -> [String]
elimina1 [] _ _ = []
elimina1 ((p:e:crd):ts) l c | (p == '*' || p == '+' || p == '!' || p == '0' || p == '1' || p == '2' || p == '3')  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = elimina ts l c
               | otherwise = [(p:e:crd)]++ elimina1 ts l c 
