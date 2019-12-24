module Main where

import System.Random
import Data.Char
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Bomberman 

-- | Uma representação do estado do jogo.
-- * Dimensões do mapa 
-- * Posição dos jogadores no mapa 
-- * imagem dos jogadores 
-- * Tempo passado desde o Inicio do jogo 
type Estado = ([String],[String],Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Float,Float)
type Estadostring = (String,Picture,Picture,Picture) 
-- | O estado inicial do jogo.
estadoInicial :: IO Estado
estadoInicial = do
    link <- loadBMP "models/link60.bmp"
    bowser <- loadBMP "models/bowser.bmp"
    buu <- loadBMP "models/Buu.bmp"
    itachi <- loadBMP "models/itachi.bmp"
    bomba <- loadBMP "models/bomb.bmp"
    pedra <- loadBMP "models/pedra.bmp"
    tijolo <- loadBMP "models/brick.bmp"
    grass <- loadBMP "models/grass.bmp"
    puflame <- loadBMP "models/puflame.bmp"
    pubomb <- loadBMP "models/pubomb.bmp"
    flame <- loadBMP "models/flame.bmp"
    x <- randomsize
    y <- randomIO   
    return ((mapa x y),((mapa x y) ++ ["0 1 1"]++["1 1 "++show (x-2)]++["2 "++show (x-2)++" 1"]++["3 "++show (x-2)++" "++show (x-2)]),link,bowser,buu,itachi,bomba,pedra,tijolo,grass,puflame,pubomb,flame,(timeleft x),0) where xP x = fromIntegral x 

timeleft :: Int -> Float
timeleft x = (conta $ fromIntegral x)*50  

powerUponly :: [String] -> [String] 
powerUponly [] = []
powerUponly (x:xs) | head x == '+' || head x == '!' = x:xs
                   | otherwise = powerUponly xs  

randomsize :: IO Int 
randomsize = do 
               x <- getStdRandom $ randomR (5,19)
               if odd x then return x
                        else randomsize           
-- | Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado e = if length (procuraplayer e) == 1 then Pictures $ [Scale 2.0 2.0 (colocaPlayer (head(head(procuraplayer e))) (players e) (-40,20))] ++ [Scale 0.5 0.5 (Text $ "WINNER!")]  
                                                   else Pictures $ ajusta e $ desenhaMapa (fundo e (getsize e) 0) 0 0 60 (getsize e) ++ desenhaMapa (auxdesenhaEstado e) 0 0 60 (getsize e) ++ colocaBombapU e (powerUps e) ++ colocaBombapU e (bombas e) ++ listaPlayer e (procuraplayer e) ++ [mostratempo e] 
           where 
           procuraplayer (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = listadosp jogo 
           
           bombas (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = bombamapa jogo     
        
           powerUps (mapa,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) =  naoapanhado (lpUdestapados (powerUponly mapa) jogo) jogo 

           mostratempo (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = Translate (-(((getsize(m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c))*60)/2) + ((((getsize(m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c))+1)*60))) ((((getsize(m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c))*60/2) - 60)) (Scale 0.5 0.5 $ Text $ show $ round $ tempo)

           players (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (link,bowser,buu,itachi)

listaPlayer :: Estado -> [String] -> [Picture]
listaPlayer _ [] = []
listaPlayer (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) (x:xs) = colocaPlayer (head x) (link,bowser,buu,itachi) (posBomb (getsize (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)) x) : listaPlayer (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) xs

colocaPlayer :: Char -> (Picture,Picture,Picture,Picture) -> (Float,Float) -> Picture
colocaPlayer x (link,bowser,buu,itachi) (xP,yP) | x == '0' = Translate xP yP link
                                                | x == '1' = Translate xP yP bowser    
                                                | x == '2' = Translate xP yP buu 
                                                | x == '3' = Translate xP yP itachi
conta :: Float -> Float
conta a = if a == 7 then 1 else (1 + conta(a - 2))

ajusta :: Estado -> [Picture] -> [Picture]
ajusta _ [] = []
ajusta e (x:xs) = scale ((1 -((conta (getsize e))*0.07))) ((1 -((conta (getsize e))*0.07))) x : ajusta e xs

findString :: [String] -> Int -> String
findString [] _ = ""  
findString list 0 = head list
findString (x:xs) a = findString xs (a - 1)

naoapanhado :: [String] -> [String] -> [String]  
naoapanhado (x:xs) (y:ys) | head x == head y = if auxnaoapanhado x y then  x : naoapanhado xs (x:ys) else naoapanhado xs (y:ys)
                          | otherwise = naoapanhado (x:xs) ys
                     where auxnaoapanhado :: String -> String -> Bool
                           auxnaoapanhado x y = if (head  (drop 2 x) == head (drop 2 y)) && (head (drop 4 x) == head (drop 4 y)) then True else False     
naoapanhado _ _ = []
 
lpUdestapados :: [String] -> [String] -> [String]
lpUdestapados [] _ = []
lpUdestapados (x:xs) jogo = if destapado x jogo then x : lpUdestapados xs jogo  
                                                else lpUdestapados xs jogo
destapado :: String -> [String] -> Bool  
destapado str [] = False 
destapado str estado | auxdestapado (findString estado ((read (linhap (drop 2 str)) :: Int))) ((read (colunap (drop 2 str)) :: Int)) == ' ' = True 
                     | otherwise = False
               where auxdestapado :: String -> Int -> Char
                     auxdestapado [] _ = ' '  
                     auxdestapado (x:xs) 0 = x
                     auxdestapado (x:xs) n = auxdestapado xs (n - 1)


posBomb :: Float -> String -> (Float,Float)
posBomb k str = ((((-((k*60)/2) + ((read (colunap (drop 2 str))::Float)*60)))),((k*60/2) - (((read (linhap (drop 2 str))::Float) *60)))) 

colocaBombapU :: Estado -> [String] -> [Picture]
colocaBombapU _ [] = []
colocaBombapU (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) (x:xs) | head x == '*' = desenhaBombapU b (posBomb (getsize (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)) x) : colocaBombapU (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) xs     
                                                                             | head x == '+' = desenhaBombapU pb (posBomb (getsize (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)) x) : colocaBombapU (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) xs 
                                                                             | head x == '!' = desenhaBombapU pf (posBomb (getsize (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)) x) : colocaBombapU (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) xs

desenhaBombapU :: Picture -> (Float,Float) -> Picture             
desenhaBombapU bomba (xB,yB) = Translate xB yB bomba
 
fundo :: Estado -> Float -> Float -> [Picture]
fundo (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) s k = if (s*s) == k then [] else g : fundo (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) s (k+1)   
 
getsize :: Estado -> Float
getsize ([],_,_,_,_,_,_,_,_,_,_,_,_,_,_) = 0
getsize ((x:xs),jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = fromIntegral $ length x  

auxdesenhaEstado :: Estado -> [Picture]
auxdesenhaEstado (_,[],_,_,_,_,_,_,_,_,_,_,_,_,_) = [] 
auxdesenhaEstado (m,(x:xs),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = if elem '#' (concat (x:xs)) then picturematch (x,p,t,g) ++ auxdesenhaEstado (m,xs,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) 
                                                                                                         else []
                                                                       where picturematch :: Estadostring -> [Picture]
                                                                             picturematch ([],_,_,_) = []
                                                                             picturematch ((x:xs),pedra,tijolo,grass) | x == '#' = pedra : picturematch (xs,pedra,tijolo,grass)
                                                                                                                      | x == '?' = tijolo : picturematch (xs,pedra,tijolo,grass)
                                                                                                                      | x == ' ' = grass : picturematch (xs,pedra,tijolo,grass)                                                                                        
                                                                                                                      | otherwise = picturematch (xs,pedra,tijolo,grass) 

--mapa em pictures, linha, coluna, pixelsize das imagens e tamanho do mapa 
desenhaMapa :: [Picture] -> Float -> Float -> Float -> Float -> [Picture]
desenhaMapa [] _ _ _ _ = [] 
desenhaMapa (x:xs) l c ps k | c == (k - 1) = Translate (-((k*ps)/2) + (c*ps)) ((k*ps/2) - (l*ps)) x : desenhaMapa xs (l+1) 0 ps k 
                            | otherwise = Translate ((-((k*ps)/2)) + (c*ps)) ((k*ps/2) - (l*ps)) x : desenhaMapa xs l (c+1) ps k
 

-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 0 'U'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) -- Player 0 
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 0 'D'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 0 'L'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 0 'R'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (Char 'b') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 0 'B'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (Char 'w') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 3 'U'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) -- Player 3  
reageEvento (EventKey (Char 's') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 3 'D'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (Char 'a') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 3 'L'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (Char 'd') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 3 'R'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento (EventKey (Char 'x') Down _ _) (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c) = (m,(move jogo 3 'B'),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,tempo,c)
reageEvento _ s = s

-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado 
reageTempo n (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,time,c) | length (listadosp jogo) == 1 = (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,time,c)
                                                                    | (2*n) < c = (m,(avanca jogo (round(time - n))),link,bowser,buu,itachi,b,p,t,g,pf,pb,f,(time-n),0)
                                                                    | otherwise = (m,jogo,link,bowser,buu,itachi,b,p,t,g,pf,pb,f,(time-n),c+n)   
                                                                                                      
-- | Frame rate
fr :: Int
fr = 50 

-- | Display mode
dm :: Display
dm = InWindow "Bomberman" (800, 600) (0, 0)
    
-- | Função principal que invoca o jogo.
main :: IO ()
main = do inicio <- estadoInicial     
          play dm              -- display mode
               (greyN 0.7)     -- côr do fundo da janela
               fr              -- frame rate
               inicio          -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo

