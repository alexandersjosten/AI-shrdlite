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
instance Eq PDDL where
    (Primative n11 n12) == (Primative n21 n22) = n11 == n21 && n12 == n22
--(ontop a b), (ontop b floor-n)

data Form = Brick | Plank | Table | Box | Pyramid | Ball | Floor
			deriving (Show)
-- Maybe use Eq to add constraints ? for example, Brick > Ball = True
-- So Brick can suport Ball
data Size = Large | Small
			deriving (Eq,Ord, Show)
type Color = String

data Object = Obj Id Form Size Color

instance Show Object where
	show (Obj i f s c) =  "[Obj " ++ i ++ " " ++ show f  ++ " " ++ show s ++ " " ++ c ++ "]"

listOfObjects :: [Object]
listOfObjects = [Obj "a" Brick   Large "green"
				,Obj "b" Brick   Small "white"
				,Obj "c" Plank   Large "red"
				,Obj "d" Plank   Small "green"
				,Obj "e" Ball    Large "white"
				,Obj "f" Ball    Small "black"
				,Obj "g" Table   Large "blue"
				,Obj "h" Table   Small "red"
				,Obj "i" Pyramid Large "yellow"
				,Obj "j" Pyramid Small "red"
				,Obj "k" Box     Large "yellow"
				,Obj "l" Box     Small "red"
				,Obj "m" Box     Large "blue"
				]

instance Show PDDL where
	show (Primative i1 i2) = "ontop " ++ i1 ++ " "  ++ i2

-- main = print $ checkGoal (Primative "l" "g") (convertWorld 0 [["e"],["g","l"],[],["k","m","f"],[]]) 
-- Returns the first id of a PDDL          
getId :: PDDL -> Id
getId (Primative a _) = a           

-- Returns object from id
getObjId :: Id -> Object
getObjId id = getO id listOfObjects
        where 
            getO id ((Obj i f s c):objs) | id == i = Obj i f s c
                                         | otherwise = getO id objs 
 
okMove :: Object -> Object -> Bool
okMove _                        (Obj _ Ball _ _ )        = False    -- Balls can't support anything.
okMove (Obj i1 Plank s1 c1)     (Obj i2 Box s2 c2)       = s1 < s2 -- Boxes cannot contain  planks  of the same size.
okMove (Obj i1 Pyramid s1 c1)   (Obj i2 Box s2 c2)       = s1 < s2 -- Boxes cannot contain pyramids of the same size.
okMove (Obj i1 Box Large c1)    (Obj i2 Brick Large c2)  = True     -- but large boxes can also be supported by large bricks.
okMove (Obj i1 Box _ c1)    	(Obj i2 Brick _ c2)  	 = False 	-- Boxes cannot be supported by bricks if both not large
okMove (Obj i1 Box s1 c1)       (Obj i2 Table s2 c2)     = s1 == s2 -- Boxes can only be supported by tables of the same size.
okMove (Obj i1 Box s1 c1)       (Obj i2 Plank s2 c2)     = s1 == s2 -- Boxes can only be supported by planks of the same size.
okMove (Obj i1 Ball s1 c1)      (Obj i2 Floor s2 c2)     = True     -- Balls must be on the floor, otherwise they roll away. 
okMove (Obj i1 Ball s1 c1)      (Obj i2 Box s2 c2)       = s1 <= s2 -- Balls must be in a box,     otherwise they roll away. 
okMove (Obj _ _ s1 _)           (Obj _ _ s2 _)           = s1 < s2  -- Small objects cannot support large objects.


{-

    -The floor can support any number of objects.
    -All objects must be supported by something.
    The arm can only hold one object at the time.
    -The arm can only pick up free objects.
    -Objects are “in” boxes, but “on” other objects.
    -Balls must be in boxes or on the floor, otherwise they roll away.
    -Balls cannot support anything.
    -Small objects cannot support large objects.
    -Boxes cannot contain pyramids or planks of the same size.
    -Boxes can only be supported by tables or planks of the same size, but large boxes can also be supported by large bricks.

-}

checkGoal :: PDDL -> PDDLWorld-> Bool
checkGoal g w = or $ map (elem g) w 


--Add object to the top of stack i and return the new world
drop' :: Int -> PDDLWorld -> Id -> PDDLWorld
drop' i  w  id = newWorld
        where
          stack = (w !! i)
          fstItemid = case stack of 
                      [] -> Primative id ("floor-" ++ (show i))
                      b  -> do
                            let obj1 = getObjId id
                            let obj2 = getObjId oldId
                            if (okMove obj1  obj2) then (Primative id oldId)  else Primative "ERROR" "ERROR" --Check if illegal move is about to be made
          oldId = getId (head stack)
          (l,r) = splitAt i w
          newWorld = l ++[(fstItemid : stack)] ++ tail r

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

-- An idea to set up the world in PDDL form
convertWorld :: Int -> World -> PDDLWorld
convertWorld n []     = []
convertWorld n (c:cs) = (reverse (createPDDL n c)):convertWorld (n+1) cs
			where
				createPDDL k [] = []
				createPDDL k (x:xs) = 
				 Primative x ("floor-" ++ (show k)) :[Primative (c !! (i+1)) (c !! i)
				  | i <-[0..length xs -1]]
				  
convertObjects :: Objects -> [Object]
convertObjects o = undefined

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err
