{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node{
    stare_arg :: s,
    actiune_arg :: Maybe a,
    parinte_arg :: Maybe (Node s a),
    depth_arg :: Int,
    cost_arg :: Float,
    copii_arg :: [Node s a]
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    Node stare1 _ _ _ _ _ == Node stare2 _ _ _ _ _ = stare1 == stare2

instance Ord s => Ord (Node s a) where
    Node stare1 _ _ _ _ _ <= Node stare2 _ _ _ _ _ = stare1 <= stare2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState nod = stare_arg nod

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent nod = parinte_arg nod

nodeDepth :: Node s a -> Int
nodeDepth nod = depth_arg nod

nodeChildren :: Node s a -> [Node s a]
nodeChildren nod = copii_arg nod

nodeHeuristic :: Node s a -> Float
nodeHeuristic nod = cost_arg nod

nodeAction :: Node s a -> Maybe a
nodeAction nod = actiune_arg nod

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}
createChildren :: (ProblemState s a, Eq s) => s -> Maybe a -> Int -> Maybe(Node s a) -> Node s a
createChildren tabla actiune nivel tata = k 
    where
        k = Node tabla actiune tata nivel (h tabla) copii
        copii = foldl (\acc (x,y) -> acc ++ [(createChildren y (Just x) (nivel + 1) (Just k))]) [] (successors tabla)
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createChildren initialState Nothing 0 Nothing


{-data Node s a = Node{
    stare_arg :: s,
    actiune_arg :: Maybe a,
    parinte_arg :: Maybe (Node s a),
    depth_arg :: Int,
    cost_arg :: Float,
    copii_arg :: [Node s a]
}-}
{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = foldl (\acc x -> if (S.member (stare_arg x) visited == False) then acc ++[x] else acc ) [] (copii_arg node) --(createChildren (stare_arg node) (actiune_arg node) (depth_arg node) node))

--createChildren tabla actiune nivel tata

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith min node (cost_arg node + (fromIntegral (depth_arg node))) frontier --undefined -- newFrontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (\acc x -> (insertSucc acc x) ) frontier lista_el_de_adaugat --undefined --newFrontier
    where
        lista_el_de_adaugat = suitableSuccs node visited

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = if (isGoal(stare_arg nod_extras) == True) then nod_extras else astar' vector_visited_actualizat (insertSuccs nod_extras (snd (deleteFindMin frontier)) vector_visited_actualizat)
    where
        nod_extras = fst(deleteFindMin frontier)
        stare_nod_extras = stare_arg nod_extras
        vector_visited_actualizat = S.insert stare_nod_extras visited 

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (PQ.insert initialNode (cost_arg initialNode) PQ.empty)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode
    |isNothing (parinte_arg goalNode) == True = []
    |otherwise = (extractPath (fromJust(parinte_arg goalNode))) ++ [((fromJust(actiune_arg goalNode)),(stare_arg goalNode))] 