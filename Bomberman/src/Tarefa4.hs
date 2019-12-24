module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import Data.Char

main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

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
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeR (elimina lst l c) (l-k) c 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeU lst (l+1) c (k+1) r
           | k == r = explodeR (removePlayer (timeOne lst l c) l c) (l-k) c 0 r

explodeR :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeR [] _ _ _ _ = []
explodeR lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = explodeD (killMap lst 0 l c) l (c-k) 0 r
           | k < r && (checkSpot lst l c) == '*' = explodeR (removePlayer (timeOne lst l c) l c) l (c+1) (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeR (removePlayer lst l c) l (c+1) (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeD (elimina lst l c) l (c-k) 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeR lst l (c+1) (k+1) r
           | k == r = explodeD (removePlayer (timeOne lst l c) l c) l (c-k) 0 r

explodeD :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeD [] _ _ _ _ = []
explodeD lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = explodeL (killMap lst 0 l c) (l+k) c 0 r
           | k < r && (checkSpot lst l c) == '*' = explodeD (removePlayer (timeOne lst l c) l c) (l-1) c (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeD (removePlayer lst l c) (l-1) c (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = explodeL (elimina lst l c) (l+k) c 0 r
           | k < r && (checkSpot lst l c) == 'n' = explodeD lst (l-1) c (k+1) r
           | k == r = explodeL (removePlayer (timeOne lst l c) l c) (l+k) c 0 r

explodeL :: [String] -> Int -> Int -> Int -> Int -> [String]
explodeL [] _ _ _ _ = []
explodeL lst l c k r | k <= r && ((checkMap lst 0 l c) == '#' || (checkMap lst 0 l c) == '?') = killMap lst 0 l c
           | k < r && (checkSpot lst l c) == '*' = explodeL (removePlayer (timeOne lst l c) l c) l (c-1) (k+1) r
           | k < r && isDigit (checkSpot lst l c) = explodeL (removePlayer lst l c) l (c-1) (k+1) r
           | k <= r && ((checkSpot lst l c) == '+' || (checkSpot lst l c) == '!') = (elimina lst l c)
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
criaCrdR lst k lng l c cnt | c /= (lng-k) = (criaCrdR (elimina (runMap lst 0 l c) l c ) lng k l (c+1) (cnt-1))
               | c == (lng-k) = (criaCrdD (elimina (runMap lst 0 l c) l c ) lng (k) (l+1) (c) (cnt-1))


criaCrdD :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdD lst _ _ _ _ 0 = lst
criaCrdD lst k lng l c cnt | l /= c = (criaCrdD (elimina (runMap lst 0 l c) l c ) lng k (l+1) (c) (cnt-1))
               | l == c = (criaCrdL (elimina (runMap lst 0 l c) l c ) lng k (l) (c-1) (cnt-1))

criaCrdL :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdL lst _ _ _ _ 0 = lst
criaCrdL lst k lng l c cnt | c /= (k+1) = (criaCrdL (elimina (runMap lst 0 l c) l c ) lng k (l) (c-1) (cnt-1))
               | c == (k+1) = (criaCrdU (elimina (runMap lst 0 l c) l c ) lng (k+1) (l-1) (c) (cnt-1))


criaCrdU :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
criaCrdU lst _ _ _ _ 0 = lst
criaCrdU lst k lng l c cnt | l /= (c+1) = (criaCrdU (elimina (runMap lst 0 l c) l c ) lng k (l-1) (c) (cnt-1))
               | l == (c+1) = (criaCrdR (elimina (runMap lst 0 l c) l c ) lng k (l) (c+1) (cnt-1))

--         estado    linha  linhaV colunaV
runMap :: [String] -> Int -> Int -> Int -> [String]
runMap [] l lv cv = []
runMap (hs:ts) l lv cv | l==lv = ((runMapTwo hs l 0 lv cv):ts)
             | otherwise = (hs:(runMap ts (l+1) lv cv))

runMapTwo :: String -> Int -> Int -> Int -> Int -> String
runMapTwo [] l c lv cv = []
runMapTwo (h:t) l c lv cv | c == cv = ('#':t)
              | otherwise = (h:runMapTwo t l (c+1) lv cv)

elimina :: [String] -> Int -> Int  -> [String]
elimina [] _ _ = []
elimina ((p:e:crd):ts) l c | (p == '*' || p == '+' || p == '!' || p == '0' || p == '1' || p == '2' || p == '3')  && (tira (tail (mantem crd))) == (show l) && (tira crd) == (show c) = elimina ts l c
               | otherwise = [(p:e:crd)]++ elimina ts l c 

mantem ::String -> String
mantem "" = ""
mantem (hs:ts) | hs == ' ' = (hs:ts)
         | otherwise = mantem ts

tira :: String -> String
tira "" = ""
tira (hs:ts) | hs /= ' ' = hs:tira ts
       | hs == ' ' = ""