{-|
  = Tarefa 1
-}

module Main where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random 

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

main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"
