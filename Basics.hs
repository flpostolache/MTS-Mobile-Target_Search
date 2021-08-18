{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
--import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    target_arg :: [Target],
    hunter_arg :: Position,
    obstacole_arg :: [Position],
    gateway_arg :: [(Position,Position)],
    nr_linii_arg :: Int,
    nr_coloane_arg :: Int
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString (Game target hunter obstacole gateway nr_linii nr_coloane)= lista_dupa_adaugare_target
    where
        lista_dupa_adaugare_target = foldl (\acc (Target (x,y) _) ->  (take ((x * (nr_coloane + 1) + y)) acc) ++ ['*'] ++ (drop ((x * (nr_coloane + 1) + y + 1)) acc)) lista_dupa_adaugare_hunter target
        lista_dupa_adaugare_hunter = (take (( (fst hunter) * (nr_coloane + 1) + (snd hunter))) lista_dupa_adaugare_gateway_secund) ++ ['!'] ++ (drop (((fst hunter) * (nr_coloane + 1) + (snd hunter) + 1)) lista_dupa_adaugare_gateway_secund)
        lista_dupa_adaugare_gateway_secund = foldl (\acc ((_,_),(z,t)) -> (take ((z * (nr_coloane + 1) + t)) acc) ++ ['#'] ++ (drop ((z * (nr_coloane + 1) + t + 1)) acc)) lista_dupa_adaugare_gateway_prim gateway
        lista_dupa_adaugare_gateway_prim = foldl (\acc ((x,y),(_,_)) -> (take ((x * (nr_coloane + 1) + y)) acc) ++ ['#'] ++ (drop ((x * (nr_coloane + 1) + y + 1)) acc)) lista_dupa_adaugare_borduri gateway
        lista_dupa_adaugare_borduri = foldl (\acc (x,y) -> (take ((x * (nr_coloane + 1) + y)) acc) ++ ['@'] ++ (drop ((x * (nr_coloane + 1) + y + 1)) acc)) lista_dupa_adaugare_spatii obstacole
        lista_dupa_adaugare_spatii = foldl (\acc (x,y) -> (take ((x * (nr_coloane + 1) + y)) acc) ++ ['\n'] ++ (drop ((x * (nr_coloane + 1) + y + 1)) acc)) lista_goala lista_pozitii_cu_spatii
        lista_pozitii_cu_spatii = zipWith (,) [0..(nr_linii - 2)] (cycle [nr_coloane])
        lista_goala = take numar_elemente (repeat ' ')
        numar_elemente = nr_linii * (nr_coloane + 1) - 1

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame a b = Game [] (1,1) obstacole1 [] a b
    where
        lista_el_linii = [0..(a - 1)]
        coloane_pe_care_adaug = [0, b -1]
        lista_el_coloane = [0..(b - 1)]
        linii_pe_care_adaug = [0, a - 1]
        obstacole1 = zipWith (,) lista_el_linii (cycle coloane_pe_care_adaug) ++ zipWith (,) lista_el_linii (cycle ( reverse coloane_pe_care_adaug )) ++ zipWith (,) (cycle linii_pe_care_adaug) lista_el_coloane ++ zipWith (,) (cycle (reverse linii_pe_care_adaug)) lista_el_coloane
        

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (a,b) (Game target hunter obstacole gateway nr_linii nr_coloane)
    | a > 0 && a < nr_linii && b > 0 && b < nr_coloane = Game target (a,b) obstacole gateway nr_linii nr_coloane
    | otherwise = Game target hunter obstacole gateway nr_linii nr_coloane

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget comportament pozitie@(a,b) (Game target hunter obstacole gateway nr_linii nr_coloane)
    | a > 0 && a < nr_linii && b > 0 && b < nr_coloane = Game ([Target pozitie comportament] ++ target) hunter obstacole gateway nr_linii nr_coloane
    | otherwise = Game target hunter obstacole gateway nr_linii nr_coloane
{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway teleporter@(((x,y),(z,t))) (Game target hunter obstacole gateway nr_linii nr_coloane)
    | x > 0 && x < nr_linii && z > 0 && z < nr_linii && y > 0 && y < nr_coloane && t > 0 && t < nr_coloane = Game target hunter obstacole (gateway ++ [teleporter]) nr_linii nr_coloane
    | otherwise = Game target hunter obstacole gateway nr_linii nr_coloane

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle  pozitie@((a,b)) (Game target hunter obstacole gateway nr_linii nr_coloane)
    | a > 0 && a < nr_linii && b > 0 && b < nr_coloane = Game target hunter (obstacole ++ [pozitie]) gateway nr_linii nr_coloane
    | otherwise = Game target hunter obstacole gateway nr_linii nr_coloane

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove t@(_,_) (Game _ _ obstacole gateway _ _)
    | elem t obstacole == True = Nothing
    | elem t poz_start_gateway == True || elem t poz_end_gateway == True = Just (foldl (\acc (prima_poz, snd_poz) -> if (prima_poz == t || snd_poz == t) then (if (prima_poz == t) then snd_poz else prima_poz) else  acc) (-1,-1) gateway)
    | otherwise = Just t
    where
        poz_start_gateway = foldl(\acc element -> acc ++ [fst element]) [] gateway
        poz_end_gateway = foldl(\acc element -> acc ++ [snd element]) [] gateway

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

goWay :: Int -> Int -> Behavior
goWay poz_x poz_Y t@(x,y) joc@(Game _ _ _ gateway _ _)
    | attemptMove (x+poz_x, y+poz_Y) joc /= Nothing = Target (fromJust (attemptMove (x+poz_x, y+poz_Y) joc)) (goWay poz_x poz_Y)
    | elem t poz_start_gateway == True || elem t poz_end_gateway == True = Target (foldl (\acc (prima_poz, snd_poz) -> if (prima_poz == t || snd_poz == t) then (if (prima_poz == t) then snd_poz else prima_poz) else  acc) (-1,-1) gateway) (goWay poz_x poz_Y)
    | otherwise = Target t (goWay poz_x poz_Y)
    where
        poz_start_gateway = foldl(\acc element -> acc ++ [fst element]) [] gateway
        poz_end_gateway = foldl(\acc element -> acc ++ [snd element]) [] gateway
goEast :: Behavior
goEast = goWay 0 1 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest = goWay 0 (-1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth = goWay (-1) 0

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth = goWay 1 0

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce dir (a,b) joc@(Game _ _ _ _ _ _)
    | dir == 1 && (attemptMove (a + 1, b) joc) /= Nothing = Target (fromJust (attemptMove (a + 1, b) joc)) (bounce 1)
    | dir == 1 && (attemptMove (a + 1, b) joc) == Nothing = Target (fromJust (attemptMove (a - 1, b) joc)) (bounce (-1))
    | dir == -1 && (attemptMove (a - 1, b) joc) /= Nothing = Target (fromJust (attemptMove (a - 1, b) joc)) (bounce (-1))
    | {- dir == -1 && (attemptMove (a - 1, b) joc) == Nothing-} otherwise = Target (fromJust (attemptMove (a + 1, b) joc)) (bounce 1)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets joc = joc{target_arg = map (\t -> (behavior t) (position t) joc) (target_arg joc) }

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (a,b) tinta 
    | a == (fst (position tinta)) && b+1  == (snd (position tinta)) = True
    | a == (fst (position tinta)) && b-1  == (snd (position tinta)) = True
    | a+ 1 == (fst (position tinta)) && b == (snd (position tinta)) = True
    | a-1  == (fst (position tinta)) && b == (snd (position tinta)) = True
    | otherwise = False
   

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir check joc@(Game _ (a,b) _ _ _ _)
    | dir == East && check == False && (attemptMove (a, b + 1)  joc) /= Nothing = joc {hunter_arg = (fromJust (attemptMove (a, b + 1)  joc) ) }
    | dir == West && check == False && (attemptMove (a, b - 1)  joc) /= Nothing = joc {hunter_arg = (fromJust (attemptMove (a, b - 1)  joc) ) }
    | dir == North && check == False && (attemptMove (a - 1, b)  joc) /= Nothing = joc {hunter_arg = (fromJust (attemptMove (a - 1, b)  joc) ) }
    | dir == South && check == False && (attemptMove (a + 1, b)  joc) /= Nothing = joc {hunter_arg = (fromJust (attemptMove (a + 1, b)  joc) ) }
    | dir == East && check == True && (attemptMove (a, b + 1)  joc) /= Nothing = joc_dupa_eliminare_target_a_doua_oara
    | dir == West && check == True && (attemptMove (a, b - 1)  joc) /= Nothing = joc_dupa_eliminare_target_a_doua_oara
    | dir == North && check == True && (attemptMove (a - 1, b)  joc) /= Nothing = joc_dupa_eliminare_target_a_doua_oara
    | dir == South && check == True && (attemptMove (a + 1, b)  joc) /= Nothing = joc_dupa_eliminare_target_a_doua_oara
    | otherwise = joc
    where
        pozitie_viitoare_hunter = if dir == East then (a, b + 1) else if(dir == West) then (a, b -1) else if(dir == North) then (a - 1, b) else (a+1,b)
        joc_cu_hunter_mutat = joc {hunter_arg = (fromJust (attemptMove pozitie_viitoare_hunter  joc) ) }
        joc_dupa_eliminare_target_o_data = joc_cu_hunter_mutat {target_arg = foldl (\acc x -> if (isTargetKilled pozitie_viitoare_hunter x == True) then acc else acc ++[x]) [] (target_arg joc_cu_hunter_mutat)}
        joc_dupa_mutare_target = moveTargets joc_dupa_eliminare_target_o_data
        joc_dupa_eliminare_target_a_doua_oara = joc_dupa_mutare_target {target_arg = foldl (\acc x -> if (isTargetKilled (hunter_arg joc_dupa_mutare_target) x == True) then acc else acc ++[x]) [] (target_arg joc_dupa_mutare_target)}
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft joc = not(null(target_arg joc))

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = foldl (\acc x-> [(x,(advanceGameState x False game))]++acc) [] [East, West, North, South]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = foldl (\acc x -> if ((isTargetKilled (hunter_arg game) x) == True) then True else acc ) False (target_arg game)

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
        where
            pow = 2 :: Int
            pozitie_actuala_hunter = hunter_arg game
            pozitie_actuala_target = position (head (target_arg game))
            x1 = fst pozitie_actuala_hunter
            x2 = fst pozitie_actuala_target
            y1 = snd pozitie_actuala_hunter
            y2 = snd pozitie_actuala_target

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
