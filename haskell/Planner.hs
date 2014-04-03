module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List


import HelpFunctions



solve :: World -> Id -> Objects -> PDDL -> Plan
solve world holding objects goal = concat [[" pick " ++ show x, "drop " ++ show y] | (x,y)<-allMoves]
    where
        allMoves = fst $ runDfs goal (convertWorld 0 world)

-- Breth first search, bad version
runDfs :: PDDL -> PDDLWorld -> ([Move],PDDLWorld)
runDfs g w = safeHead $ filter (/= ([],[])) [ dfs i g w [] [w] | i<-[0..]]


-- Starts going down the left most tree. Depth-first-search, with depth level
dfs :: Int -> PDDL -> PDDLWorld -> [Move] -> [PDDLWorld] ->  ([Move],PDDLWorld)
dfs 0 _ w ms _   = ([],[])
dfs d g w ms wos =  do
        let wos' = bfsStep (ms,w) (w:wos)
        let goal = checkAllW wos'
        if goal == [] then safeHead $ filter (/= ([],[])) [dfs (d-1) g (snd i) (fst i) ((snd i):wos) | i<-wos'] else head goal 
            where
                checkAllW [] = []
                checkAllW ((mvs,wo):rest)  | checkGoal g wo =  [(mvs,wo)]
                                           | otherwise = checkAllW rest
--print $ dfs 5 (Primative "e" "floor-4") testWorld [] [testWorld]                                       
          
-- Return head of list, if list is  empty returns ([].[])                                 
safeHead :: [([Move],PDDLWorld)] -> ([Move],PDDLWorld)
safeHead lst | lst == [] = ([],[])
			 | otherwise = head lst
                                           
-- Goes one level down in the tree and return list of Worlds and the moves to get to that world
bfsStep :: ([Move],PDDLWorld) -> [PDDLWorld]->  [([Move],PDDLWorld)]
bfsStep (mvs,w) ws = [sim i  | i <- (getAllMove w), snd (sim i) `notElem` ws ]
                     where
                        sim (x,y) = do
                             let (i,nw) = take' x w
                             let nw'    = drop' y nw i
                             (mvs ++ [(x,y)],nw')
