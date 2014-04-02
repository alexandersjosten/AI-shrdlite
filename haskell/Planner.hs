module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List


import HelpFunctions



solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = ["I picked it up . . .", "pick " ++ show col, ". . . and I dropped it down", "drop " ++ show col]
    where
      Just col = findIndex (not . null) world


-- Starts going down the left most tree. Depth-first-search
dfs :: PDDL -> PDDLWorld -> [Move] -> [PDDLWorld] ->  ([Move],PDDLWorld)
dfs g w ms wos =  do
        let wos' = bfsStep (ms,w) (w:wos)
        let goal = checkAllW wos'
        if goal == [] then head [dfs g (snd i) (fst i) ((snd i):wos) | i<-wos'] else head goal 
            where
                checkAllW [] = []
                checkAllW ((mvs,wo):rest)  | checkGoal g wo =  [(mvs,wo)]
                                           | otherwise = checkAllW rest
                                           
                                           
                                           
-- Goes one level down in the tree and return list of Worlds and the moves to get to that world
bfsStep :: ([Move],PDDLWorld) -> [PDDLWorld]->  [([Move],PDDLWorld)]
bfsStep (mvs,w) ws = [sim i  | i <- (getAllMove w), snd (sim i) `notElem` ws ]
                     where
                        sim (x,y) = do
                             let (i,nw) = take' x w
                             let nw'    = drop' y nw i
                             (mvs ++ [(x,y)],nw')
