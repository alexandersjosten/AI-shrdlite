module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List
import Data.Char
import Data.Maybe


import HelpFunctions



-- Case for holding objects and goals when only taking up objects is next.
solve :: World -> Id -> Objects -> [PDDL] -> Plan
solve world holding objects goal = concat [[" pick " ++ show x, "drop " ++ show y] | (x,y)<-allMoves]
    where
        allMoves = fst $ runDfs goal world

-- Breth first search, bad version
runDfs :: [PDDL] -> World -> ([Move],PDDLWorld)
runDfs g w = safeHead $ filter (/= ([],[])) [ dfs i ss g wP [] [wP] | i<-[(sDepth)..]]
				where
					stuff = map (findStuff w) g
					(sDepth,ss) = maximum' stuff
					wP = (convertWorld w)


-- Heuristics for solving goles
-- Only works for OnTop now.
-- (minDepth,(smallerStack,BiggerStack))
findStuff :: World -> PDDL -> (Int,(Int,Int))
findStuff w (PDDL Inside a b)  = findStuff w (PDDL Ontop a b)
findStuff w (PDDL Ontop a b) 
					| length b > 1 =  do 
							 let s2      = head . map snd . take 1 . sort $ zip w [0..] -- b is a floor
							 let h2      = length (w !! s2)
							 let (s1,h1) = findSAH a w
							 returnV (s1,h1) (s2,h2)
					| otherwise   = do
							 let (s1,h1) = findSAH a w
							 let (s2,h2) = findSAH b w
							 returnV (s1,h1) (s2,h2)
					where
			returnV (s1',h1') (s2',h2') | s1'==s2'  =  if h1'>h2' then (h1'+1,(s2',s1')) else (h2'+1 ,(s1',s2'))
										| otherwise = if h1'>h2' then (h1'+h2'+1,(s2',s1')) else (h1'+h2'+1,(s1',s2'))
findStuff w (PDDL Above a b) = do
                            let (s1,h1) = findSAH a w
                            let (s2,_) = findSAH b w
                            (h1+1,(s2,s1)) -- Maybe modify the smaller stack
findStuff w (PDDL Under a b) = findStuff w (PDDL Above b a)
findStuff w (PDDL Leftof a b) = do
                            let (s1,h1) = findSAH a w
                            let (s2,h2) = findSAH b w
                            getSuff (s1,h1) (s2,h2)
                    where 
                    getSuff (s1',h1') (s2',h2') 
                        | s1' == s2' = if h1'> h2' then (h2'+1,(s1',s2')) else (h1'+1, (s1',s2'))
                        | s2' == 0 && s1' == length w = ((h2'+1)+(h1'+1),(s1',s2'))
                        | s2' == 0 = (h2'+1,(s2',s2'))
                        | s1' < s2' = (0,(0,0))
                        | otherwise = if h1' < h2' then (h1'+1,(s2',s1')) else (h2'+1,(s1',s2'))
findStuff w (PDDL Rightof a b) = findStuff w (PDDL Leftof b a)
findStuff w (PDDL Beside a b) = do 
                        let (s1,h1) = findSAH a w
                        let (s2,h2) = findSAH b w
                        getSuff (s1,h1) (s2,h2)
                    where 
                    getSuff (s1',h1') (s2',h2') 
                        | s1' == s2' = if h1'> h2' then (h2'+1,(s1',s2')) else (h1'+1, (s1',s2'))
                        | s2' == 0 = (h1'+1,(1,s1'))
                        | s2' == ((length w) - 1) = (h1'+1,((length w -1),s1'))
                        |otherwise  = do
                                        let left  = length (w !! (s2'-1))
                                        let right = length (w !! (s2'+1))
                                        if right > left then (h1'+1,(s2' -1,s1')) else (h1'+1,(s2' + 1,s1'))



-- Starts going down the left most tree. Depth-first-search, with depth level
dfs :: Int -> (Int,Int) -> [PDDL] -> PDDLWorld -> [Move] -> [PDDLWorld] ->  ([Move],PDDLWorld)
dfs 0 _ g w ms _   = if  and (map (checkGoal w) g) then (ms,w) else ([],[])
dfs d ss g w ms wos =  do
        let wos' = bfsStep ss (ms,w) (w:wos)
        safeHead $ filter (/= ([],[])) [dfs (d-1) ss g (snd i) (fst i) ((snd i):wos) | i<-wos']
                                      
          
-- Return head of list, if list is  empty returns ([].[])                                 
safeHead :: [([Move],PDDLWorld)] -> ([Move],PDDLWorld)
safeHead lst | lst == [] = ([],[])
			 | otherwise = head lst
                                           
-- Goes one level down in the tree and return list of Worlds and the moves to get to that world
bfsStep :: (Int,Int) -> ([Move],PDDLWorld) -> [PDDLWorld]->  [([Move],PDDLWorld)]
bfsStep ss (mvs,w) ws = [k  | i <- (sortMoves ss (getAllMove w)),let k = sim i, snd (k) `notElem` ws ]
                     where
                        sim (x,y) = do
                             let (i,nw) = take' x w
                             let nw'    = drop' y nw i
                             (mvs ++ [(x,y)],nw')
                             
                             
-- Sort what stacks to start working on
sortMoves :: (Int,Int) -> [Move] -> [Move]
sortMoves (s1,s2) mv =  sortBy (sortGT s2) (sortBy (sortGT s1) mv)


--sortGT :: Int -> [Move] -> [Move]
sortGT i (a1, b1) (a2, b2)
  | a1 == i = LT
  | otherwise = GT



