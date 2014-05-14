module Heuristics where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List
import Data.Char
import Data.Maybe


import HelpFunctions


-- Heuristics for solving goles
-- Returns (minDepth,(smallerStack,BiggerStack))
heuristics :: World -> PDDL -> (Int,(Int,Int))
heuristics w (PDDL Inside a b)  = heuristics w (PDDL Ontop a b)
heuristics w (PDDL Ontop a "") = do 
			let (s1,h1) = findSAH a w
			(h1,(s1,s1))
heuristics w (PDDL Ontop a b) 
        | checkGoal (convertWorld w) (PDDL Ontop a b) = (0,(0,0))
		| b == "floor" =  do 
				 let s2      = head . map snd . take 1 . sort $ zip w [0..] -- b is a floor
				 let h2      = length (w !! s2) -1
				 let (s1,h1) = findSAH a w
				 getSuff (s1,h1) (s2,h2)
		| otherwise   = do
				 let (s1,h1) = findSAH a w
				 let (s2,h2) = findSAH b w
				 getSuff (s1,h1) (s2,h2)
		where
			getSuff (s1',h1') (s2',h2') 
				| s1'==s2'  =  if h1'>h2' then (h1'+1,(s2',s1')) else (h2'+1 ,(s1',s2'))
				| otherwise = if h1'>h2' then (h1'+h2'+1,(s2',s1')) else (h1'+h2'+1,(s1',s2'))
heuristics w (PDDL Above a b) 
        | checkGoal (convertWorld w) (PDDL Above a b) = (0,(0,0))
        | otherwise = do
        		let (s1,h1) = findSAH a w
        		let (s2,_) = findSAH b w
        		(h1+1,(s2,s1)) -- Maybe modify the smaller stack
heuristics w (PDDL Under a b) = heuristics w (PDDL Above b a)
heuristics w (PDDL Leftof a b)  
        | checkGoal (convertWorld w) (PDDL Leftof a b) = (0,(0,0))
        | otherwise = do
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
heuristics w (PDDL Rightof a b) = heuristics w (PDDL Leftof b a)
heuristics w (PDDL Beside a b)  
        | checkGoal (convertWorld w) (PDDL Beside a b) = (0,(0,0))
        | otherwise = do
		let (s1,h1) = findSAH a w
		let (s2,h2) = findSAH b w
		getSuff (s1,h1) (s2,h2)
	where 
	getSuff (s1',h1') (s2',h2') 
		| s1' == s2' = if h1'> h2' then (h2'+1,(s1',s2')) else (h1'+1, (s1',s2'))
		| s2' == 0 = (h1'+1,(1,s1'))
		| s2' == ((length w) - 1) = (h1',((length w -1),s1'))
		|otherwise  = do
						let left  = length (w !! (s2'-1))
						let right = length (w !! (s2'+1))
						if right > left then (h1',(s2' -1,s1')) else (h1',(s2' + 1,s1'))
