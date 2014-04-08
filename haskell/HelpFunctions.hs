module HelpFunctions where 

import Text.JSON
import Data.List
import Data.Maybe

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
data PDDL = PDDL Relation Id Id


-- With this Eq, we can check if a PDDLworld == PDDLworld
instance Eq PDDL where
    (PDDL Ontop n11 n12) == (PDDL Ontop n21 n22) = n11 == n21 && n12 == n22
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
  show (PDDL Beside  i1 i2) = "beside "  ++ i1 ++ " " ++ i2
  show (PDDL Leftof  i1 i2) = "leftof "  ++ i1 ++ " " ++ i2
  show (PDDL Rightof i1 i2) = "rightof " ++ i1 ++ " " ++ i2
  show (PDDL Above   i1 i2) = "above "   ++ i1 ++ " " ++ i2
  show (PDDL Ontop   i1 i2) = "ontop "   ++ i1 ++ " " ++ i2
  show (PDDL Under   i1 i2) = "under "   ++ i1 ++ " " ++ i2
  show (PDDL Inside  i1 i2) = "inside "  ++ i1 ++ " " ++ i2

-- Find stack number and hight from Id
findSAH :: Id -> World -> (Int,Int)		
findSAH id w =  do
					let stacknr = head $ elemIndices False (map isNothing (map (findIndex (id==)) w))
					let height  = head $ findIndices (id==) (reverse (w !! stacknr)) 
					(stacknr,height)

-- Returns the first id of a PDDL, the one that is on top        
getId :: PDDL -> Id
getId (PDDL Ontop a _) = a           

-- Returns object from id
getObjId :: Id -> Object
getObjId id = case lookup id listOfObjects of
                    Nothing -> error("Error in getObjId ")
                    Just a  -> a
                 
                 
-- Returns the Object that is on top in a Ontop
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


-- Check if the world contains the goal, handles spatial relations
checkGoal ::  PDDLWorld-> PDDL -> Bool
checkGoal w (PDDL Ontop a b)   = or $ map (elem (PDDL Ontop a b)) w 
checkGoal w (PDDL Inside a b)  = or $ map (elem (PDDL Ontop a b)) w 
checkGoal w (PDDL Above a b)   = aboveOrUnder (a,b) (convertPDDLWorld w)
checkGoal w (PDDL Under a b)   = aboveOrUnder (b,a) (convertPDDLWorld w)
checkGoal w (PDDL Beside a b)  = besideOf (a,b) (convertPDDLWorld w)
checkGoal w (PDDL Leftof a b)  = leftOrRightOf (a,b) (convertPDDLWorld w)
checkGoal w (PDDL Rightof a b) = leftOrRightOf (b,a) (convertPDDLWorld w)
        


-- If the first ID is above the secound, then return true
aboveOrUnder :: (Id,Id) -> World -> Bool
aboveOrUnder (t,u) w = do
                let (s1,a) = findSAH  t w
                let (s2,b) = findSAH  u w
                s1 == s2 && a<b

-- if the first ID is left of the secound, then return true
leftOrRightOf :: (Id,Id) -> World -> Bool
leftOrRightOf (t,u) w = do
                let (a,_) = findSAH  t w
                let (b,_) = findSAH  u w
                a<b

-- If Ids are in adjecent stacks, then return true
besideOf :: (Id,Id) -> World -> Bool
besideOf (t,u) w = do 
        let (a,_) = findSAH  t w
        let (b,_) = findSAH  u w
        abs (a - b) == 1 
        
--Add object to the top of stack i and return the new world
drop' :: Int -> PDDLWorld -> Id -> PDDLWorld
drop' i  w  id = newWorld
        where
          stack = (w !! i)
          fstItemid = case stack of 
                      [] -> PDDL Ontop id ("floor")
                      b  -> PDDL Ontop id (getId (head stack))
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


-- Convert world from given form to PDDL form
convertWorld :: World -> PDDLWorld
convertWorld []     = []
convertWorld (c:cs) = (reverse (createPDDL c)):convertWorld  cs
			where
				createPDDL [] = []
				createPDDL(x:xs) = 
				 PDDL Ontop x ("floor") :[PDDL Ontop (c !! (i+1)) (c !! i)
				  | i <-[0..length xs -1]]
				  
-- Convert world from PDDL form to given form
convertPDDLWorld :: PDDLWorld -> World
convertPDDLWorld [] = []
convertPDDLWorld (c:cs) = (reverse (createPDDL c)):convertPDDLWorld  cs
			where
				createPDDL [] = []
				createPDDL((PDDL Ontop a _):xs) =[a] ++ createPDDL xs
        
-- Takes list of moves and a world, and returns a new world after the moves have been made
convertMoveToWorld :: [Move] -> PDDLWorld -> PDDLWorld
convertMoveToWorld [] w         = w
convertMoveToWorld ((x,y):m) w  = do
                     let (i,nw) = take' x w
                     let nw'    = drop' y nw i
                     convertMoveToWorld m nw'
                     
-- http://stackoverflow.com/questions/18118280/finding-maximum-element-in-a-list-of-tuples                    
maximum' :: Ord t => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | m < (fst p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps


-- test worlds
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
