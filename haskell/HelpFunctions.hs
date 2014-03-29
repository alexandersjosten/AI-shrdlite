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



-- An idea to set up the world in PDDL form
convertWorld :: World -> PDDLWorld
convertWorld []     = []
convertWorld (c:cs) = (reverse (createPDDL c)):convertWorld cs
			where
				createPDDL [] = []
				createPDDL  (x:xs) = 
				 Primative x "floor":[Primative (c !! (i+1)) (c !! i)
				  | i <-[0..length xs -1]]


ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err
