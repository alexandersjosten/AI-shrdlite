module HelpFunctions where 


import Text.JSON
import Data.List (findIndex)
import ShrdliteGrammar

type Objects = JSObject JSValue
type Utterance = [String]
type Id = String
type Index = Int
type World = [[Id]]
type PDDLWorld = [[PDDL]]
type Goal = Bool
type Plan = [String]
type Move = (Int,Int) -- (Pick int, Drop int)

-- First, divide the features that describe the world into primitive 
-- and derived features. Definite clauses are 
data PDDL = Primative Id Id
instance Eq PDDL where
    (Primative n11 n12) == (Primative n21 n22) = n11 == n21 && n12 == n22
--(ontop a b), (ontop b floor-n)

listOfObjects :: [(Id,Object)]
listOfObjects = [("a", Object Large Green Brick)
				,("b", Object Small White Brick) 
				,("c", Object Large Red Plank)  
				,("d", Object Small Green Plank) 
				,("e", Object Large White Ball) 
				,("f", Object Small Black Ball)   
				,("g", Object Large Blue Table)  
				,("h", Object Small Red Table)
				,("i", Object Large Yellow Pyramid)
				,("j", Object Small Red Pyramid)
				,("k", Object Large Yellow Box)
				,("l", Object Large Red Box)
				,("m", Object Small Blue Box) 
				]



instance Show PDDL where
	show (Primative i1 i2) = "ontop " ++ i1 ++ " "  ++ i2

-- main = print $ (convertWorld 0 [["e"],["g","l"],[],["k","m","f"],[]]) 
-- Returns the first id of a PDDL          
getId :: PDDL -> Id
getId (Primative a _) = a           

-- Returns object from id
getObjId :: Id -> Object
getObjId id = case lookup id listOfObjects of
                    Nothing -> error("Error in getObjId ")
                    Just a  -> a
                    
getIdPrim :: PDDL -> Object
getIdPrim pd = getObjId $ getId pd
 
okMove :: Object -> Object -> Bool
okMove _                       (Object _ _ Ball)        = False    -- Balls can't support anything.
okMove (Object s1 _ Plank)     (Object s2 _  Box)       = s1 < s2  -- Boxes cannot contain  planks  of the same size.
okMove (Object s1 _ Pyramid)   (Object s2 _ Box)        = s1 < s2  -- Boxes cannot contain pyramids of the same size.
okMove (Object Large _ Box)    (Object Large _  Brick)  = True     -- but large boxes can also be supported by large bricks.
okMove (Object s1 _ Box)       (Object s2 _ Brick)  	= False    -- Boxes cannot be supported by bricks if both not large
okMove (Object s1 _ Box)       (Object s2 _ Table)      = s1 == s2 -- Boxes can only be supported by tables of the same size.
okMove (Object s1 _ Box)       (Object s2 _ Plank)      = s1 == s2 -- Boxes can only be supported by planks of the same size.
okMove (Object s1 _ Ball)      (Object s2 _ AnyForm)    = True     -- Balls must be on the floor, otherwise they roll away. 
okMove (Object s1 _ Ball)      (Object s2 _ Box)        = s1 <= s2 -- Balls must be in a box,     otherwise they roll away. 
okMove (Object s1 _ Ball)      (Object s2 _ Table)      = False    -- Balls must be in a box,     otherwise they roll away.
okMove (Object s1 _ _)         (Object s2 _  _)         = s1 < s2  -- Small objects cannot support large objects.



getAllMoveAll :: PDDLWorld -> [(Int,Int)]
getAllMoveAll ws = concat [ zip (repeat i) (getAllMove (getIdPrim (head (ws !! i))) ws) |  i<-[0..length ws -1] , (ws !! i) /= []]

getAllMove :: Object -> PDDLWorld -> [Int]
getAllMove o ws  = [ i | i<-[0..length ws -1] , (ws !! i)  == []  || o `okMove` getIdPrim (head (ws !! i)) ]



-- main = print $ getAllMove (getObjId "k") (convertWorld 0 [["e"],["g","l"],[],["k","m","f"],[]]) 

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
