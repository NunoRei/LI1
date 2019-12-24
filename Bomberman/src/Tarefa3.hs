{-|
  = Tarefa 3
-}

module Main where

import System.Environment
import Data.List
import Data.Char (isDigit,isSpace) 


{-|
  Função que recebe um estado de jogo em forma de lista de Strings, e a 
  comprime na forma de String. 

    == Utilização:
    
    >>> encode  ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
    "   ?      ? ?  ? ?  ?  ???? + 3 3! 5 5* 7 7 1 1 10p0 4 3 +p1 7 7"
-}

encode :: [String] -> String
encode l = retirapedras (retiraespacofixo (sizemapa l) (sizemapa l) (separaPlayers l))

{-|
  Função que recebe a String resultante da compressão realizada pela
  função encode, e a descomprime para a forma de lista de Strings de 
  modo a que essa lista de Strings seja exatamente igual ao estado de 
  jogo inicial. 

    == Utilização:
    
    >>> decode  "   ?      ? ?  ? ?  ?  ???? + 3 3! 5 5* 7 7 1 1 10p0 4 3 +p1 7 7"
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
-}

decode :: String -> [String]
decode l = pedraEspaco (tamanho (length (separaString l)) 5) (tamanho (length (separaString l)) 5) (separaString l) ++ listapUbombas (stringpUbombas (resto l)) ++ listaplayers (stringplayers (resto l))
    where tamanho :: Int -> Int -> Int
          tamanho x k = if (ncasas k == x) then k else tamanho x (k + 2)

{-|
  Função que recebe o estado de jogo na forma de lista de Strings,
  e vai calcular o tamanho do mapa através do tamanho da primeira 
  String. 

    == Utilização:
    
    >>> sizemapa  ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
    9
-}

sizemapa :: [String] -> Int 
sizemapa [] = 0
sizemapa (h:t) = length h

{-|
  Função que dado um estado de jogo, coloca um 'p' em cada string
  correspondente a um jogador para que esta possa mais tarde ser separada
  no decode.

    == Utilização:

    >>> separaPlayers ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
    ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","p0 4 3 +","p1 7 7"]
-}


separaPlayers :: [String] -> [String]
separaPlayers [] = []
separaPlayers ((h:t):r) | h == '0' || h == '1' || h == '2' || h == '3' = (('p':h:t): (separaPlayers r))
                        | otherwise = ((h:t): (separaPlayers r))

{-|
  Função que recebe dois inteiros, ambos são o tamanho do mapa mas um vai
  funcionar como contador, e o estado de jogo em forma de lista de Strings,
  e vai retirar os espaços vazios que são sempre fixos.

    == Utilização:
    
    >>> retiraespacofixo 9 9 ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
    "##########   ###?# ###     ? ##?# # #?## ?  ?  ###?#?###?? ##########+ 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
-}

retiraespacofixo :: Int -> Int -> [String] -> String 
retiraespacofixo _ _ [] = ""
retiraespacofixo d k (h:t) | (d - 1) == k = retira2espacofixo d 1 h ++ retiraespacofixo d (k-1) t 
                           | (d - 2) == k = retira1espacofixo d 1 h ++ retiraespacofixo d (k-1) t      
                           | k == 3 = retira1espacofixo d 1 h ++ retiraespacofixo d (k-1) t
                           | k == 2 = retira2espacofixo d 1 h ++ retiraespacofixo d (k-1) t
                           | k == 0 = h ++ retiraespacofixo d 0 t 
                           | otherwise = h ++ retiraespacofixo d (k-1) t

{-|
  Função que recebe dois inteiros,o tamanho do mapa e um contador a começar em 1,
  (vindo da função retiraespacofixo) e uma String. A função vai remover os 
  primeiros dois espaços vazios, e os últimos dois espaços vazios.

    == Utilização:
    
    >>> retira2espacofixo 9 1 "#       #"
    "#   #"
-}

retira2espacofixo :: Int -> Int -> String -> String
retira2espacofixo _ _ [] = [] 
retira2espacofixo d k (h:t) | k == 2 = retira2espacofixo d (k + 1) t
                            | k == 3 = retira2espacofixo d (k + 1) t
                            | k == (d - 2) = retira2espacofixo d (k + 1) t
                            | k == (d - 1) = retira2espacofixo d (k + 1) t 
                            | otherwise = h : retira2espacofixo d (k + 1) t

{-|
  Função que recebe dois inteiros,o tamanho do mapa e outro inteiro,
  (vindo da retiraespacofixo) e uma String. A função vai remover o 
  primeiro espaço vazio, e o último espaço vazio da String. 

    == Utilização:
    
    >>> retira1espacofixo 9 1 "# #?#?# #"
    "##?#?##"
-}

retira1espacofixo :: Int -> Int -> String -> String
retira1espacofixo _ _ [] = []
retira1espacofixo d k (h:t) | k == 2 = retira1espacofixo d (k + 1) t
                            | k == (d - 1) = retira1espacofixo d (k + 1) t 
                            |otherwise = h : retira1espacofixo d (k + 1) t

{-|
  Função que recebe uma String, e vai remover todas as ocorrências
  do carácter '#'. 

    == Utilização:
    
    >>> retirapedras "##########   ###?# ###     ? ##?# # #?## ?  ?  ###?#?###?? ##########+ 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
    "   ?      ? ?  ? ?  ?  ???? + 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
-}

retirapedras :: String -> String
retirapedras [] = []
retirapedras (h:t) = if h == '#' then retirapedras t else h : retirapedras t

{-|
  Função que recebe uma String, e remove tudo o que aparece 
  apartir de um cáracter que não seja ou espaço ou '?'.  

    == Utilização:
    
    >>> separaString "   ?      ? ?  ? ?  ?  ???? + 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
    "   ?      ? ?  ? ?  ?  ???? "
-}

separaString :: String -> String
separaString [] = ""
separaString (h:t) = if ( isSpace h || h == '?') then h : separaString t else separaString []

{-|
  Função que recebe uma String, e devolve tudo o que aparece a seguir
  ao carácter '+', inclusive.

    == Utilização:
    
    >>> resto "   ?      ? ?  ? ?  ?  ???? + 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
    "+ 3 3! 5 5* 7 7 1 1 100 4 3 +1 7 7"
-}

resto :: String -> String 
resto [] = ""
resto (h:t) = if (h == ' ' || h == '?') then resto t else h : t

{-|
  Função que recebe uma String, e devolve tudo o que aparece a seguir 
  à primeira ocorência do carácter 'p'.

    == Utilização:
    
    >>> stringplayers "+ 3 3! 5 5* 7 7 1 1 10p0 4 3 +p1 7 7"
    "0 4 3 +p1 7 7"
-}

stringplayers :: String -> String 
stringplayers [] = ""
stringplayers (x:y:ys) | (x == 'p') = y : ys 
                       | ys == "" = ""
                       | otherwise = stringplayers (y:ys)

{-|
  Função que recebe uma String, e devolve uma lista de Strings, 
  separando a String inicial em várias Strings sempre que o 
  carácter 'p' aparece.

    == Utilização:
    
    >>> listaplayers "0 4 3 +p1 7 7"
    ["0 4 3 +","1 7 7"]
-}

listaplayers :: String -> [String]
listaplayers [] = []
listaplayers list = take (auxconta list) list : listaplayers (drop ((auxconta list) + 1) list) 
            where auxconta :: String -> Int
                  auxconta [] = 0
                  auxconta (x:xs) = if (x == 'p') then 0 else 1 + auxconta xs

{-|
  Função que recebe uma String, e devolve tudo o que aparece antes 
  da primeira ocorência do carácter 'p'.

    == Utilização:
    
    >>> stringpUbombas "+ 3 3! 5 5* 7 7 1 1 10p0 4 3 +p1 7 7"
    "+ 3 3! 5 5* 7 7 1 1 10"
-}

stringpUbombas :: String -> String 
stringpUbombas [] = ""
stringpUbombas (x:y:ys) | (y == 'p') = x : "" 
                        | ys == "" = x : y : ""
                        | otherwise = x : stringpUbombas (y:ys)

{-|
  Função que recebe uma String, e devolve uma lista de Strings, 
  separando a String inicial em várias Strings sempre que o 
  carácter não é ou um dígito, ou um espaço.

    == Utilização:
    
    >>> listapUbombas "+ 3 3! 5 5* 7 7 1 1 10"
    ["+ 3 3","! 5 5","* 7 7 1 1 10"]
-}

listapUbombas :: String -> [String]
listapUbombas [] = []
listapUbombas (x:xs) | x == 'p' = []  
listapUbombas list = take ((auxconta2 (drop 1 list)) + 1) list : listapUbombas (drop ((auxconta2 (drop 1 list)) + 1) list)
             where auxconta2 :: String -> Int 
                   auxconta2 [] = 0
                   auxconta2 (x:xs) = if (isDigit x || isSpace x) then 1 + auxconta2 xs else 0                                            

{-|
  Função que põe as casas predefinidas, ou seja, as paredes, os espaços nos cantos
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
  = Função dada previamente que testa as funções encode e decode.
-}

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"
