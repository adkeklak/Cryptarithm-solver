import System.Environment
import System.IO

type Mapping = [(Char, Char)]

-- wszystkie podlisty
podlisty :: [Char] -> [[Char]]
podlisty [] = [[]]
podlisty (x:xs) = [x:podlista | podlista <- podlisty xs] ++ podlisty xs

-- sprawndzenie czy lista ma dana dlugosc
czyListaJestDlugosci :: Int -> [Char] -> Bool
czyListaJestDlugosci len sublist = len == (length sublist)

-- podlisty "0123456789" danej dlugosci
podlistyDlugosci :: Int -> [[Char]]
podlistyDlugosci length = filter (czyListaJestDlugosci length) (podlisty ['0'..'9'])

-- permutacje listy
permutacje :: [Char] -> [[Char]]
permutacje [] = [[]]
permutacje xs = [x : rest | x <- xs, rest <- permutacje (filter (/= x) xs)]

-- permutacje listy danej dlugosci
permutacjeList :: Int -> [[Char]]
permutacjeList n = foldl (\acc x -> (permutacje x)++acc) [] (podlistyDlugosci n)

-- usuwa kopie znakow
znaki :: String -> String
znaki [] = []
znaki (x:xs) = x : znaki (filter (/= x) xs)

-- usuawa +,=,* z listy znakow
usunZnaki :: String -> String
usunZnaki [] = []
usunZnaki (x:xs)
  | x `elem` ['+', '=', '*'] = usunZnaki xs
  | otherwise = x : usunZnaki xs

-- znajduje rozne znaki ze stringa baz +,=,*
rozneLitery :: String -> String
rozneLitery t = usunZnaki ( znaki t)

-- znajdz wszystie mapowania dla tekstu
listyPar :: String -> [Mapping]
listyPar text = map (zip (rozneLitery text)) (permutacjeList (length (rozneLitery text))) 

-- zamien znaki na cyfry uzywanjac danego mapowania 
zamienZnaki :: String -> Mapping -> (String,Mapping)
zamienZnaki text mapping = ((foldl zamienZnak text mapping),mapping)
    where
        zamienZnak text (oldChar, digit) = map (\c -> if c == oldChar then digit else c) text

-- znajdz wszystie napisy utworzone po zastosowaniu wszystkich permutacji
zamienTeksty :: String -> [(String,Mapping)]
zamienTeksty text = map (\x -> zamienZnaki text x) (listyPar text)

-- podziel tekst na lewa i prawa stone rownia
podzielStrony :: String -> (String, String)
podzielStrony text = doPary (words ( map (\c -> if c == '=' then ' ' else c) text))
  where
    doPary :: [String] -> (String,String)
    doPary [left,right] = (left,right)

-- dzieli napis na liczby i mnozenia
podzielNaMnozenia :: String -> [String]
podzielNaMnozenia text = words ( map (\c -> if c == '+' then ' ' else c) text)

-- dzieli napis z mnozeniem na poszczegulne liczby
podzielNaLiczby :: String -> [String]
podzielNaLiczby text = words ( map (\c -> if c == '*' then ' ' else c) text)

-- dzieli caly napis na liczby
podzielNaWszystko :: String -> [String]
podzielNaWszystko text = words ( map (\c -> if c `elem` ['+', '=', '*'] then ' ' else c) text)

-- zamineia napisz na liczby jednoczesnie monzac liczby jezeli wystepuje mnozenie 
mnozenia :: String -> Integer
mnozenia text = product (map (\t -> read t :: Integer) (podzielNaLiczby text))

-- dodaje wszystie elementy w liscie
dodajStrone :: [String] -> Integer
dodajStrone list = sum (map (mnozenia) list)

-- sprawdza czy suma lewo i prawa strona rownia sa rowne
porownajStrony :: ([String], [String]) -> Bool
porownajStrony (left,right) = (dodajStrone left) == (dodajStrone right)

--pierwszy element
first :: (a,b) -> a
first (fst,_) = fst

--drugi element
second :: (a,b) -> b
second (_,snd) = snd

--spwadza czy rownianie jest prawdziwe
czyRownianiePrawdziwe :: String -> Bool
czyRownianiePrawdziwe text = porownajStrony (podzielNaMnozenia (first (podzielStrony text)) , podzielNaMnozenia (second (podzielStrony text)))

-- sprawdza czy napis zaczyna sie od 0 nie wliczajac samego zera
czyZaczynaSieOdZera :: String -> Bool
czyZaczynaSieOdZera "0" = False
czyZaczynaSieOdZera ('0':_) = True
czyZaczynaSieOdZera _ = False

-- sprawdza czy któraś liczba nie jest zerem
czyNieZaczynajaSieOdZera :: String -> Bool
czyNieZaczynajaSieOdZera text = null ( filter (czyZaczynaSieOdZera) (podzielNaWszystko text))

--sprawdza czy mapowanie jest poprawne
sprawdzPoprawnosc :: String -> Bool
sprawdzPoprawnosc text = (czyRownianiePrawdziwe text) && (czyNieZaczynajaSieOdZera text)

--znajdowanie wszystich rozwiazania
znajdzRozwiazania :: String -> [(String,Mapping)]
znajdzRozwiazania text = filter (\p -> sprawdzPoprawnosc (first p)) (zamienTeksty text)

--zapisanie mapowania w stringu
mapowanieDoString :: Mapping -> String
mapowanieDoString [] = ""
mapowanieDoString ((x, y):xs) = x : '=' : y : ' '  : mapowanieDoString xs

--wypisuje jedno roziazanie
rozwiazanie :: [(String,Mapping)] -> String
rozwiazanie [] = "Brak rozwiazan"
rozwiazanie ((text, mapping):xs) = mapowanieDoString mapping
--rozwiazanie ((text, mapping):xs) =text ++ "    " ++ (mapowanieDoString mapping)

klytarytmy :: Handle -> IO ()
klytarytmy handle = do
  eof<-hIsEOF handle
  if eof then return ()
  else do
    line<-hGetLine handle
    let roz = rozwiazanie (znajdzRozwiazania line)
    putStrLn $ line ++ "    " ++ roz
    klytarytmy handle

main = do
  (firstArg:_) <-getArgs
  fileHandle <-openFile firstArg ReadMode
  klytarytmy fileHandle
  hClose fileHandle