module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List
import Data.Char
import Data.Maybe


import HelpFunctions



solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = concat [[" pick " ++ show x, "drop " ++ show y] | (x,y)<-allMoves]
    where
        allMoves = fst $ runDfs (Primative "e" "floor-1") world

-- Breth first search, bad version
runDfs :: PDDL -> World -> ([Move],PDDLWorld)
runDfs g w = safeHead $ filter (/= ([],[])) [ dfs i ss g wP [] [wP] | i<-[(sDepth)..]]
				where
					(sDepth,ss) = findStuff w g
					wP = (convertWorld 0 w)

-- (minDepth,(smallerStack,BiggerStack))
findStuff :: World -> PDDL -> (Int,(Int,Int))
findStuff w (Primative a b) 
					| length b > 1 =  do 
							 let s2      = digitToInt (last b) -- b is a floor
							 let h2      = length (w !! s2)
							 let (s1,h1) = findSAH a
							 returnV (s1,h1) (s2,h2)
					| otherwise   = do
							 let (s1,h1) = findSAH a
							 let (s2,h2) = findSAH b
							 returnV (s1,h1) (s2,h2)
		where
			findSAH id = do
					let stacknr = head $ elemIndices False (map isNothing (map (findIndex (id==)) w))
					let height  = head $ findIndices (id==) (reverse (w !! stacknr))
					(stacknr,height)
			returnV (s1',h1') (s2',h2') = if h1'>h2' then (h1'+h2'+2,(s2',s1')) else (h1'+h2'+2,(s1',s2'))

			


-- Starts going down the left most tree. Depth-first-search, with depth level
dfs :: Int -> (Int,Int) -> PDDL -> PDDLWorld -> [Move] -> [PDDLWorld] ->  ([Move],PDDLWorld)
dfs 0 _ _ w ms _   = ([],[])
dfs d ss g w ms wos =  do
        let wos' = bfsStep ss (ms,w) (w:wos)
        let goal = checkAllW wos' 
        if goal == [] then safeHead $ filter (/= ([],[])) [dfs (d-1) ss g (snd i) (fst i) ((snd i):wos) | i<-wos'] else head goal 
            where
                checkAllW [] = []
                checkAllW ((mvs,wo):rest)  | checkGoal g wo =  [(mvs,wo)]
                                           | otherwise = checkAllW rest
--print $ runDfs (Primative "e" "floor-4") testWorld                                       
          
-- Return head of list, if list is  empty returns ([].[])                                 
safeHead :: [([Move],PDDLWorld)] -> ([Move],PDDLWorld)
safeHead lst | lst == [] = ([],[])
			 | otherwise = head lst
                                           
-- Goes one level down in the tree and return list of Worlds and the moves to get to that world
bfsStep :: (Int,Int) -> ([Move],PDDLWorld) -> [PDDLWorld]->  [([Move],PDDLWorld)]
bfsStep (s1,s2) (mvs,w) ws = [k  | i <- (sortMoves s2 (getAllMove w)),let k = sim i, snd (k) `notElem` ws ]
                     where
                        sim (x,y) = do
                             let (i,nw) = take' x w
                             let nw'    = drop' y nw i
                             (mvs ++ [(x,y)],nw')
                             
                             
                             
sortMoves :: Int -> [Move] -> [Move]
sortMoves ss mv = sortBy (sortGT ss) mv

--sortGT :: Int -> [Move] -> [Move]
sortGT i (a1, b1) (a2, b2)
  | a1 == i = LT
  | otherwise = GT
