{-|
	= Tarefa 6
-}

module Tarefa6_li1g088 where

import Data.List
import System.Environment
import Data.Char
import Data.Maybe

{-|
	Função que dado um estado de jogo, um inteiro correspondente ao número de jogador e um inteiro correspondente aos
	instantes de tempo que faltam para terminar o jogo, devolve um Maybe Char para mover o jogador, que pode ser Just 'U',
	Just 'D', Just 'L', ou Just 'R', colocar uma bomba, Just 'B', ou não fazer nenhuma ação, Nothing.

		== Utilização:

		>>> bot ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","+ 3 3","! 5 5","0 4 3 +","1 4 5", "2 3 3", "3 3 7"] 2 20
		Just 'R'
-}

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = checkBomb (sepBombs mapa) mapa (checkPlayerL mapa player) (checkPlayerC mapa player) player

{-|
	Função que dado um estado de jogo e um número de jogador, devolve um inteiro com a linha onde este se encontra.

	== Utilização:

	>>> checkPlayerL ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","+ 3 3","! 5 5","0 4 3 +","1 4 5", "2 3 3", "3 3 7"] 0
	3
-}

checkPlayerL :: [String] -> Int -> Int
checkPlayerL [] l = 0
checkPlayerL (h:ts) plr = if (take 1 h) == (show plr) then linha h
						  else checkPlayerL ts plr

{-|
	Função que dado um estado de jogo e um número de jogador, devolve um inteiro com a coluna onde este se encontra.

	== Utilização:

	>>> checkPlayerC ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","+ 3 3","! 5 5","0 4 3 +","1 4 5", "2 3 3", "3 3 7"] 0
	4
-}

checkPlayerC :: [String] -> Int -> Int
checkPlayerC [] c = 0
checkPlayerC (h:ts) plr = if (take 1 h) == (show plr) then coluna h
						  else checkPlayerC ts plr


{-|
	Função que dada uma string com informação de um jogador, powerup ou bomba, devolve um inteiro com a linha onde este se encontra.

	== Utilização:

	>>> linha "* 3 4 0 1 10"
	4
-}

linha :: String -> Int
linha crd = (read (tira (tail( mantem (drop 2 crd))))::Int)

{-|
	Função que dada uma string com informação de um jogador, powerup ou bomba, devolve um inteiro com a coluna onde este se encontra.

	== Utilização:

	>>> coluna "* 3 4 0 1 10"
	3
-}

coluna  :: String -> Int
coluna crd = (read (tira (drop 2 crd)) ::Int)

{-|
	Função que dada uma string com informação de uma bomba, devolve um inteiro com o raio de explosão dela.

	== Utilização:

	>>> raio "* 3 4 0 1 10"
	1
-}

raio :: String -> Int
raio crd = (read (tira (tail (mantem (tail (mantem (tail (mantem (tail (mantem crd))))))))) :: Int)


{-|
	Função que dada uma lista de strings relativa ao estado de jogo, devolve uma lista de strings apenas com as bombas.

	== Utilização:

	>>> sepBombs ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 1 2 2 5","+ 3 3","! 5 5","0 4 3 +","1 4 5", "2 3 3", "3 3 7"]
	["* 3 4 0 1 10","* 1 1 2 2 5"]
-}

sepBombs :: [String] -> [String]
sepBombs [] = []
sepBombs (h:t) = if head h == '*' then h : (sepBombs t)
								  else sepBombs t

{-|
	Função que dada uma lista de strings relativa ao estado de jogo, uma lista de strings apenas com as bombas e três inteiros,
	com a linha do jogador, a coluna do jogador e o numero do jogador, respetivamente, trata de verificar se o jogador está a salvo
	de bombas na casa onde se encontra. Caso essa condição se verifique, chama uma função que se encarrega de fazer o jogador mover-se
	para onde não seja atingido pela espiral, e caso contrário, devolve um Maybe Char que ajuda o jogador a fugir do alcance da bomba.

	== Utilização:

	>>> checkBomb ["* 3 4 0 1 10","* 2 7 1 1 3"] ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 2 7 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 1 1
	Just 'U'
-}

checkBomb :: [String] -> [String] -> Int -> Int -> Int -> Maybe Char
checkBomb [] (h:t) l c plr = if odd (div (length h) 2) then moveOut (h:t) (div (length h) 2) (div (length h) 2) l c else moveOut (h:t) (div (length h) 2) ((div (length h) 2)-1) l c
checkBomb (h:t) lst l c plr | linha h == l && coluna h == c = if checkVertic lst l c == Nothing then (if checkHorizon lst l c == Nothing then Nothing else checkHorizon lst l c) else checkVertic lst l c
							| linha h == l && (rangeH lst h l c 1) = if checkVertic lst l c == Nothing then (if c > coluna h then Just 'R' else Just 'L') else checkVertic lst l c
							| coluna h == c && (rangeV lst h l c 1) = if checkHorizon lst l c == Nothing then (if l > linha h then Just 'D' else Just 'U') else checkHorizon lst l c
							| otherwise = checkBomb t lst l c plr

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e dois inteiros, com a linha e a coluna do jogador, 
	respetivamente, verifica se é possível o jogador mover-se na vertical. Caso essa condição se verifique em ambos os sentidos, para
	cima e para baixo, chama outra função que se encarrega de decidir que direção tomar. Caso a condição se verifique apenas num
	sentido, devolve o Maybe Char que lhe corresponde. Se ambos os sentidos estiverem bloqueados, chama uma outra função que tenta mover
	o jogador na horizontal, de modo a tentar fugir da bomba de outra forma.

	== Utilização:

	>>> checkVertic ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 2 7 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 1
	Just 'U'
-}

checkVertic :: [String] -> Int -> Int -> Maybe Char
checkVertic [] l c = Nothing
checkVertic lst l c | (checkMap lst 0 (l+1) c) == ' ' && (checkMap lst 0 (l-1) c) == ' ' = moveCenterVertic lst l
					| (checkMap lst 0 (l+1) c) == ' ' = Just 'D'
					| (checkMap lst 0 (l-1) c) == ' ' = Just 'U'
					| (checkMap lst 0 (l-1) c) /= ' ' && (checkMap lst 0 (l+1) c) /= ' ' = Nothing
					| otherwise = Nothing

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e dois inteiros, com a linha e a coluna do jogador, 
	respetivamente, verifica se é possível o jogador mover-se na horizontal. Caso essa condição se verifique em ambos os sentidos, para
	a esquerda e para a direita, chama outra função que se encarrega de decidir que direção tomar. Caso a condição se verifique apenas num
	sentido, devolve o Maybe Char que lhe corresponde. Se ambos os sentidos estiverem bloqueados, chama uma outra função que tenta mover
	o jogador na vertical, de modo a tentar fugir da bomba de outra forma.

	== Utilização:

	>>> checkHorizon ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 1
	Just 'R'
-}

checkHorizon :: [String] -> Int -> Int -> Maybe Char
checkHorizon [] l c = Nothing
checkHorizon lst l c | (checkMap lst 0 l (c+1)) == ' ' && (checkMap lst 0 l (c-1)) == ' ' = moveCenterHorizon lst c
					 | (checkMap lst 0 l (c+1)) == ' ' = Just 'R'
					 | (checkMap lst 0 l (c-1)) == ' ' = Just 'L'
					 | (checkMap lst 0 l (c+1)) /= ' ' && (checkMap lst 0 l (c-1)) /= ' ' = Nothing
					 | otherwise = Nothing

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e um inteiro com a linha do jogador,
	verifica se é mais favorável o jogador mover-se para cima ou para baixo, conforme a linha do mapa
	em que se encontra.

	== Utilização:

	>>> moveCenterVertic ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 6", "2 3 3", "3 3 7"] 6
	Just 'U'
-}

moveCenterVertic :: [String] -> Int -> Maybe Char
moveCenterVertic (h:t) l | l <= div (length h) 2 = Just 'D'
						 | l > div (length h) 2 = Just 'U'

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e um inteiro com a coluna do jogador,
	verifica se é mais favorável o jogador mover-se para a esquerda ou para a direita, conforme a coluna do
	mapa em que se encontra.

	== Utilização:

	>>> moveCenterHorizon ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 2 7 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 2 7", "2 3 3", "3 3 7"] 2
	Just 'R'
-}

moveCenterHorizon :: [String] -> Int -> Maybe Char
moveCenterHorizon (h:t) c | c <= div (length h) 2 = Just 'R'
						  | c > div (length h) 2 = Just 'L'

{-|
	Função que dada uma lista de strings relativa ao estado de jogo, uma string com a informação de uma bomba e três inteiros,
	com a linha do jogador, a coluna do jogador e um contador, respetivamente, verifica se o jogador está dentro do alcance
	horizontal da bomba na casa onde se encontra. Caso esta condição se verifique devolve True, e caso contrário devolve False.

	== Utilização:

	>>> rangeH ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 2 7 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 1 1
	True
-}

rangeH :: [String] -> String -> Int -> Int -> Int -> Bool
rangeH [] _ _ _ _ = False
rangeH lst crd l c r | coluna crd < c && (coluna crd) + r == c = True
					 | coluna crd > c && (coluna crd) - r == c = True
					 | raio crd == r = False
					 | coluna crd < c && (checkMap lst 0 l ((coluna crd) +r) /= ' ' || not (checkSafe lst l (c+r))) = False
					 | coluna crd > c && (checkMap lst 0 l ((coluna crd) -r) /= ' ' || not (checkSafe lst l (c-r))) = False
					 | coluna crd < c && (checkMap lst 0 l ((coluna crd) +r) == ' ' && checkSafe lst l (c+r)) = rangeH lst crd l c (r+1)
					 | coluna crd > c && (checkMap lst 0 l ((coluna crd) -r) == ' ' && checkSafe lst l (c-r)) = rangeH lst crd l c (r+1)

{-|
	Função que dada uma lista de strings relativa ao estado de jogo, uma string com a informação de uma bomba e três inteiros,
	com a linha do jogador, a coluna do jogador e um contador, respetivamente, verifica se o jogador está dentro do alcance
	vertical da bomba na casa onde se encontra. Caso esta condição se verifique devolve True, e caso contrário devolve False.

	== Utilização:

	>>> rangeV ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 1 1
	True
-}

rangeV :: [String] -> String -> Int -> Int -> Int -> Bool
rangeV [] _ _ _ _ = False
rangeV lst crd l c r | linha crd < l && (linha crd) + r == l = True
					 | linha crd > l && (linha crd) - r == l = True
					 | raio crd == r = False
					 | linha crd < l && (checkMap lst 0 ((linha crd) +r) c /= ' ' || not (checkSafe lst (l+r) c)) = False
					 | linha crd > l && (checkMap lst 0 ((linha crd) -r) c /= ' ' || not (checkSafe lst (l-r) c)) = False
					 | linha crd < l && (checkMap lst 0 ((linha crd) +r) c == ' ' && checkSafe lst (l+r) c) = rangeV lst crd l c (r+1)
					 | linha crd > l && (checkMap lst 0 ((linha crd) -r) c == ' ' && checkSafe lst (l-r) c) = rangeV lst crd l c (r+1)

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e três inteiros, um contador e a linha
	e coluna do jogador, respetivamente, devolve o carater do mapa correspondente às coordenadas dadas.

	== Utilização:

	>>> checkMap ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 1 1
	' '
-}

checkMap :: [String] -> Int -> Int -> Int -> Char
checkMap [] l lv cv = '#'
checkMap (hs:ts) l lv cv | l==lv = checkMapTwo hs l 0 lv cv
                    	 | otherwise = checkMap ts (l+1) lv cv

{-|
	Função que auxilia a checkMap ao receber uma stringa do estado de jogo e quatro inteiros, dois contadores e
	a linha e coluna do jogador, respetivamente, e devolve o carater do mapa correspondente às coordenadas dadas.

	== Utilização:

	>>> checkMapTwo "#   ??  #" 1 0 1 1
	' '
-}

checkMapTwo :: String -> Int -> Int -> Int -> Int -> Char
checkMapTwo [] l c lv cv = '#'
checkMapTwo (h:t) l c lv cv | c == cv = h
                        	| otherwise = checkMapTwo t l (c+1) lv cv

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e dois inteiros, a linha e a coluna do jogador,
	verifica se existe algum powerup nas coordenadas dadas que impeça a passagem das chamas de uma bomba. Caso esta
	condição se verifique devolve False, e caso contrário devolve True.

	== Utilização:

	>>> checkSafe ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 1 1
	True
-}

checkSafe:: [String] -> Int -> Int -> Bool
checkSafe [] l c = True
checkSafe ((p:crd):ts) l c | p == '+' && linha (p:crd) == l && coluna (p:crd) == c = False
					  	   | p == '!' && linha (p:crd) == l && coluna (p:crd) == c = False
					  	   | p == '*' && linha (p:crd) == l && coluna (p:crd) == c = checkSafe ts l c
					  	   | isDigit p && linha (p:crd) == l && coluna (p:crd) == c = checkSafe ts l c
					  	   | otherwise = checkSafe ts l c

{-|
	Função que dada uma lista de strings relativa ao estado de jogo, uma lista de strings com a informação das bombas e
	dois inteiros, uma linha e uma coluna, respetivamente, verifica se as coordenadas são uma localização a salvo do alcance
	das bombas. Caso a condição se verifique devolve True, caso contrário devolve False.

	== Utilização:

	>>> casaSegura ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","* 3 4 0 1 10","* 1 6 1 1 3","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 7 7
	True
-}

casaSegura:: [String] -> [String] -> Int -> Int -> Bool
casaSegura _ [] _ _ = True
casaSegura lst (h:t) l c | linha h == l && coluna h == c = False
						 | linha h == l && (rangeH lst h l c 1) = False
						 | coluna h == c && (rangeV lst h l c 1) = False
						 | otherwise = casaSegura lst t l c

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e quatro inteiros, a linha objetivo, a coluna objetivo, a linha do 
	jogador e a coluna do jogador, respetivamente, trata de fazer o jogador chegar à última casa segura do mapa, movendo-se e colocando
	bombas através dos respetivos Maybe Char até lá chegar. Esta função em particular trata dos movimentos horizontais, chamando a
	função moveOutTwo quando precisa de se mover na vertical.

	== Utilização:

	>>> moveOut ["#########","#   ??  #","# #?# # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","+ 3 3","! 5 5","0 4 3 +","1 1 7", "2 3 3", "3 3 7"] 4 3 7 1
	Just 'R'
-}

moveOut :: [String] -> Int -> Int -> Int -> Int -> Maybe Char
moveOut [] kl kc l c = Nothing
moveOut (h:t) kl kc l c | c < kc && checkMap (h:t) 0 l (c+1) == ' ' = if casaSegura (h:t) (sepBombs (h:t)) l (c+1) then Just 'R' else Nothing
						| c < kc && checkMap (h:t) 0 l (c+1) == '?' = Just 'B'
						| c < kc && checkMap (h:t) 0 l (c+1) == '#' = moveOutTwo (h:t) kl kc l c
						| c > kc && checkMap (h:t) 0 l (c-1) == ' ' = if casaSegura (h:t) (sepBombs (h:t)) l (c-1) then Just 'L' else Nothing
						| c > kc && checkMap (h:t) 0 l (c-1) == '?' = Just 'B'
						| c > kc && checkMap (h:t) 0 l (c-1) == '#' = moveOutTwo (h:t) kl kc l c
						| c == kc && l /= kl = moveOutTwo (h:t) kl kc l c
						| l == kl && c == kc = Nothing

{-|
	Função que dada uma lista de strings relativa ao estado de jogo e quatro inteiros, a linha objetivo, a coluna objetivo, a linha do 
	jogador e a coluna do jogador, respetivamente, trata de fazer o jogador chegar à última casa segura do mapa, movendo-se e colocando
	bombas através dos respetivos Maybe Char até lá chegar. Esta função em particular trata dos movimentos horizontais, chamando a
	função moveOutTwo quando precisa de se mover na vertical.

	== Utilização:

	>>> moveOut ["#########","#   ??  #","# # # # #","#  ?  ? #","# # # #?#","#    ?  #","# # #?# #","#       #","#########","+ 3 3","! 5 5","0 4 3 +","1 3 7", "2 3 3", "3 3 7"] 4 3 7 3
	Just 'U'
-}

moveOutTwo :: [String] -> Int -> Int -> Int -> Int -> Maybe Char
moveOutTwo [] kl kc l c = Nothing				
moveOutTwo (h:t) kl kc l c | l < kl && checkMap (h:t) 0 (l+1) c == ' ' = if casaSegura (h:t) (sepBombs (h:t)) (l+1) c then Just 'D' else Nothing
						   | l < kl && checkMap (h:t) 0 (l+1) c == '?' = Just 'B'
						   | l < kl && checkMap (h:t) 0 (l+1) c == '#' = moveOut (h:t) kl kc l c
						   | l > kl && checkMap (h:t) 0 (l-1) c == ' ' = if casaSegura (h:t) (sepBombs (h:t)) (l-1) c then Just 'U' else Nothing
						   | l > kl && checkMap (h:t) 0 (l-1) c == '?' = Just 'B'
						   | l > kl && checkMap (h:t) 0 (l-1) c == '#' = moveOut (h:t) kl kc l c
						   | l == kl && c /= kc = moveOut (h:t) kl kc l c
						   | l == kl && c == kc = Nothing

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
