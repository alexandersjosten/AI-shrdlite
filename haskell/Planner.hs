module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List
import Data.Char
import Data.Maybe


import HelpFunctions
import TalkingBastard
import Heuristics




-- Case for holding objects and goals when only taking up objects is next.
solve :: World -> Id -> Id -> [PDDL] -> Plan
solve world hold holding []  = ["No goal??? Interpreter ..... "] 
solve world hold holding  ((PDDL t a b):goal)
	  | not $ and $ map (\(PDDL t a b)-> okMove (getObjId a) (getObjId b)) (filter isOnTop ((PDDL t a b):goal)) = ["Not Possible!"]
	  | (b /= "") && (and $ map (checkGoal pddlWorld) ((PDDL t a b):goal)) = ["Already a goal!"]
      | hold /= "no"    = if holding == a && b=="" then [] 
                          else 
							if (b == "floor" && (length goal < 0)) then
								["I drop " ++ amIAlone holding (convertWorld world),"drop " ++ show holdMove ++ " "]
							else
								["I drop " ++ amIAlone holding (convertWorld world),"drop " ++ show holdMove ++ " "] ++ (solve newWorld "no" "" goal')
      | b == ""         = startTB allMovesT (convertWorld world) ++ [" and now I'm holding the " ++ amIAlone a pddlWorld," pick " ++ show (fst $ findSAH a world)]
      | otherwise       = startTB allMoves (convertWorld world)
    where
        maxD            = length $ concat world
        allMoves        = fst $ runBfs maxD  goal' world
        (allMovesT,nw)  = case runBfs maxD [PDDL t a ""] world of
								([],nw') -> ([(99,99)],nw') -- Special case of only one move
								b -> b
        newWorld        = convertPDDLWorld $  drop' holdMove pddlWorld holding 
        pddlWorld       = convertWorld world
        goal'           = ((PDDL t a b):goal)
        holdMoveList    = [(drop' i pddlWorld holding, i)  | i <- getObjMoves (getObjId holding) pddlWorld] 
        holdMoveGoals   = filter (/= -1) [ if checkGoal w (PDDL t a b) then i else -1 |  (w,i) <- holdMoveList]
        holdMove        = case holdMoveGoals of
                            [] -> snd $ head $ holdMoveList
                            b  -> head $ holdMoveGoals


-- Breth first search, calls bfs with increasing depth
runBfs :: Int-> [PDDL] -> World -> ([Move],PDDLWorld)
runBfs maxD g w = if sDepth>maxD then ([(sDepth,(-1))],convertWorld w)
					else safeHead $ filter (/= ([],[])) [ bfs i ss g wP [] [wP] | i<-[(sDepth)..maxD]]
				where
					heuristicsList = map (heuristics w) g
					(sDepth',ss) = maximum' heuristicsList
					sDepth = sum $ fst $ unzip heuristicsList
					wP = (convertWorld w)
					
-- Starts going down the left most tree. Depth-first-search, with depth level
bfs :: Int -> (Int,Int) -> [PDDL] -> PDDLWorld -> [Move] -> [PDDLWorld] ->  ([Move],PDDLWorld)
bfs 0 _ g w ms _   = if  and (map (checkGoal w) g) then (ms,w) else ([],[])
bfs d ss g w ms wos =  do
        let wos' =  bfsStep ss (ms,w) (w:wos)
        safeHead $ filter (/= ([],[])) [bfs (d-1) ss g (snd i) (fst i) ((snd i):wos) | i<-wos']
					
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


-- sortGT sorts specific stack first
sortGT i (a1, b1) (a2, b2)
  | a1 == i = LT
  | otherwise = GT



