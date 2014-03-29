module HelpFunctions where 


import Text.JSON
import Data.List (findIndex)

type Objects = JSObject JSValue
type Utterance = [String]
type Id = String
type Index = Int
type World = [[Id]]
type PDDLWorld = [[PDDL]]
type Goal = Bool
type Plan = [String]

-- First, divide the features that describe the world into primitive 
-- and derived features. Definite clauses are 
data PDDL = Primative Id Id
--(ontop a b), (ontop b floor-n)

instance Show PDDL where
	show (Primative i1 i2) = "ontop " ++ i1 ++ " "  ++ i2

-- Remove top object from stack i and return the object and new world
-- Stack count starts at 0
take' :: Int -> PDDLWorld -> (Id,PDDLWorld)
take' i w = (getId fstItem ,newWorld)
        where
          stack = (w !! i)
          fstItem = case stack of
                        [] -> Primative "ERROR" "ERROR"
                        b  -> head stack
          newStack = case stack of
                        [] -> []
                        b  -> tail stack
          (l,r) = splitAt i w
          newWorld = l ++ [newStack] ++ (tail r)
          
          

--kann ekki ad pattern matcha tetta er madur ad fara ad nota primative?
--þurfum eitthvað sem finnur efsta objectið, take nema ignore world >ld
getId :: PDDL -> Id
getId (Primative a b) = a             


--Add object to the top of tack i and return the new world
drop' :: Int -> PDDLWorld -> Id -> PDDLWorld
drop' i  w  id = newWorld
        where
          stack = (w !! i)
          fstItemid = case stack of 
                      [] -> Primative id ("floor-" ++ (show i))
                      b  -> Primative id (getId (head stack)) 
          (l,r) = splitAt i w
          newWorld = l ++[(fstItemid : stack)] ++ tail r-- djofull ljott


-- An idea to set up the world in PDDL form
convertWorld :: Int -> World -> PDDLWorld
convertWorld n []     = []
convertWorld n (c:cs) = (reverse (createPDDL n c)):convertWorld (n+1) cs
			where
				createPDDL k [] = []
				createPDDL k (x:xs) = 
				 Primative x ("floor-" ++ (show k)) :[Primative (c !! (i+1)) (c !! i)
				  | i <-[0..length xs -1]]

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err
