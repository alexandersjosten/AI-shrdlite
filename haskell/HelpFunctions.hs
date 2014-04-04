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


-- With this Eq, we can check if a PDDLworld == PDDLworld
instance Eq PDDL where
    (Primative n11 n12) == (Primative n21 n22) = n11 == n21 && n12 == n22
--(ontop a b), (ontop b floor-n)


-- Hardcoded object, same as we get from small.json and medium.json
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


-- Returns the first id of a PDDL, the one that is on top        
getId :: PDDL -> Id
getId (Primative a _) = a           

-- Returns object from id
getObjId :: Id -> Object
getObjId id = case lookup id listOfObjects of
                    Nothing -> error("Error in getObjId ")
                    Just a  -> a
                 
                 
-- Returns the Object that is on top in a Primative
getIdPrim :: PDDL -> Object
getIdPrim pd = getObjId $ getId pd
 
 -- Check if the move fullfills all constraints
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
okMove (Object s1 _ Ball)      (Object s2 _ Brick)      = False    -- Balls must be in a box,     otherwise they roll away.
okMove (Object s1 _ _)         (Object s2 _  _)         = s1 <= s2 -- Small objects cannot support large objects.

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

-- Get all leagal moves in the world
getAllMove :: PDDLWorld -> [Move]
getAllMove ws = concat [ zip (repeat i) (getObjMoves (getIdPrim (head (ws !! i))) ws) |  i<-[0..length ws -1] , (ws !! i) /= []]

-- Get all moves for specific object
getObjMoves :: Object -> PDDLWorld -> [Int]
getObjMoves o ws  = [ i | i<-[0..length ws -1] , (ws !! i)  == []  || o `okMove` getIdPrim (head (ws !! i)) ]


-- Check if the world contains the goal
checkGoal ::  PDDLWorld-> PDDL -> Bool
checkGoal w g = or $ map (elem g) w 


--Add object to the top of stack i and return the new world
drop' :: Int -> PDDLWorld -> Id -> PDDLWorld
drop' i  w  id = newWorld
        where
          stack = (w !! i)
          fstItemid = case stack of 
                      [] -> Primative id ("floor-" ++ (show i))
                      b  -> Primative id (getId (head stack))
          (l,r) = splitAt i w
          newWorld = l ++[(fstItemid : stack)] ++ tail r


-- Remove top object from stack i and return the object and new world
-- Stack count starts at 0
take' :: Int -> PDDLWorld -> (Id,PDDLWorld)
take' i w = (getId fstItem ,newWorld)
        where
          stack    = (w !! i)
          fstItem  = head stack
          newStack = tail stack
          (l,r)    = splitAt i w
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

            
-- Takes list of moves and a world, and returns a new world after the moves have been made
convertMoveToWorld :: [Move] -> PDDLWorld -> PDDLWorld
convertMoveToWorld [] w         = w
convertMoveToWorld ((x,y):m) w  = do
                     let (i,nw) = take' x w
                     let nw'    = drop' y nw i
                     convertMoveToWorld m nw'
                     
maximum' :: Ord t => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m < (fst p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps


medWorld :: World
medWorld = [["e"],["a","l"],[],[],["i","h","j"],[],[],["k","g","c","b"],[],["d","m","f"]]

complexWorld :: World 
complexWorld = [["e"],["a","l"],["i","h","j"],["c","k","g","b"],["d","m","f"]]

testWorld :: World
testWorld = [["e"],["g","l"],[],["k","m","f"],[]]

simpleWorld :: World
simpleWorld = [["e"],[],[],[],[]]



ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err
