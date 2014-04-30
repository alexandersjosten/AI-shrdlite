module Interpreter (
  -- Only export needed functions, rest are help functions
  interpret
) where

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Text.JSON.Types
import Data.List (elemIndex)
--import Text.Groom
import Data.Maybe
import Data.List

import HelpFunctions hiding (Floor, Box, Ball)


-- For testing only!!
exampleTable :: [(Id, Object)]
exampleTable = [ ("a", Object Large Green Brick)
               , ("b", Object Small White Brick)
               , ("c", Object Large Red Plank)
               , ("d", Object Small Green Plank)
               , ("e", Object Large White Ball)
               , ("f", Object Small Black Ball)
               , ("g", Object Large Blue Table)
               , ("h", Object Small Red Table)
               , ("i", Object Large Yellow Pyramid)
               , ("j", Object Small Red Pyramid)
               , ("k", Object Large Yellow Box)
               , ("l", Object Large Red Box)
               , ("m", Object Small Blue Box)
               ]

exampleWorld :: World
exampleWorld = [["g", "c", "a"], ["b", "d", "f"], ["e"]]
-- Internal representation of the given data, either an Object or the floor
data IntObj = Simply Object | IFloor
            deriving Show
-- Colors: Black | White | Blue | Green | Yellow | Red
colorTable :: [(String, Color)]
colorTable = [ ("black", Black)
             , ("white", White)
             , ("blue", Blue)
             , ("green", Green)
             , ("yellow", Yellow)
             , ("red", Red)
             ]

-- Sizes: Small | Large
sizeTable :: [(String, Size)]
sizeTable = [("small", Small), ("large", Large)]

-- Forms: Brick | Plank | Ball | Pyramid | Box | Table
formTable :: [(String, Form)]
formTable = [ ("brick", Brick)
            , ("plank", Plank)
            , ("ball", Ball)
            , ("pyramid", Pyramid)
            , ("box", Box)
            , ("table", Table)
            ]

interpret :: World -> Id -> Objects -> Command -> [PDDL]
interpret world holding objects tree =
  map (createPDDL os) $ translateCommand tree os
    where table = createTable objects
          os    = filterWorld world table

-- Filters all the objects from the JSON-part that doesn't exist in the world
filterWorld :: World -> [(Id, Object)] -> [(Id, Object)]
filterWorld world mapping = filter ((`elem` flatWorld) . fst) mapping
  where flatWorld = concat world
--------------------------------------------------------------------------------
--------------------------------- Lookup table ---------------------------------
--------------------------------------------------------------------------------
createTable :: Objects -> [(Id, Object)]
createTable os = createTable' (ids `zip` os')
  where a   = fromJSObject os
        ids = map fst a
        os' = map encode $ map snd a

createTable' :: [(Id, String)] -> [(Id, Object)]
createTable' []           = []
createTable' ((id, s):xs) =
  (id, Object getSize getColor getForm) : createTable' xs
  where getSize  = findStuff sizeTable  s
        getColor = findStuff colorTable s
        getForm  = findStuff formTable  s

-- We should always find something..
findStuff :: [(String, a)] -> String -> a
findStuff [] s = error $ "Couldn't match the string " ++ show s ++ " to something"
findStuff ((id, obj):xs) s
  | id `isInfixOf` s = obj
  | otherwise        = findStuff xs s

--------------------------------------------------------------------------------

translateCommand :: Command -> [(Id, Object)] -> [(IntObj, Relation, IntObj)]
translateCommand cmd objects =
  case cmd of
    Move Floor _ -> error "Can't move the floor!"
    Move e     l -> createTriple (getEntities e objects) (getLocations l objects)
    Take Floor   -> error "Can't take the floor!"
    Take e       -> createTriple (getEntities e objects) [(Ontop, IFloor)]
    Put        l -> createTriple [IFloor] (getLocations l objects)

-- RelativeEntity Quantifier Object Location
-- Special handling if we have Any quantifier, otherwise do same thing
getEntities :: Entity -> [(Id, Object)] -> [IntObj]
getEntities Floor _                   = [IFloor]
getEntities (BasicEntity q o) objects =
  case q of
    Any ->
      case translateObject o objects of
        []     -> error $ "Can't find any object matching " ++ show o
        (x:xs) -> [x]
    _   -> translateObject o objects
getEntities (RelativeEntity q o l) objects =
  case q of
    The -> translateObject o objects
    Any ->
      case translateObject o objects of
        [] -> error $ "Can't find any object matching " ++ show o
        (x:xs) -> [x]
    All -> translateObject o objects
-- Move (RelativeEntity The (Object AnySize White Ball) (Relative Inside (BasicEntity Any (Object AnySize AnyColor Box))))
--      (Relative Ontop Floor)
-- Relative Relation Entity
getLocations :: Location -> [(Id, Object)] -> [(Relation, IntObj)]
getLocations (Relative r e) objects = [(r, e')| e' <- getEntities e objects]

createTriple :: [IntObj] -> [(Relation, IntObj)] -> [(IntObj, Relation, IntObj)]
createTriple xs ys = [(o1, r, o2) | o1 <- xs, (r, o2) <- ys]

translateObject :: Object -> [(Id, Object)] -> [IntObj]
translateObject (Object AnySize AnyColor f) =
  map Simply . filter (\(Object _  _  f') -> f == f') . map snd
translateObject (Object AnySize c        f) =
  map Simply . filter (\(Object _  c' f') -> c == c' && f == f') . map snd
translateObject (Object s       AnyColor f) =
  map Simply . filter (\(Object s' _  f') -> s == s' && f == f') . map snd
translateObject (Object AnySize c        AnyForm) =
  map Simply . filter (\(Object _ c' _) -> c == c') . map snd
translateObject (Object s       c        f)
  = map Simply . filter (\(Object s' c' f') -> s == s' && c == c' && f == f') . map snd

createPDDL :: [(Id, Object)] -> (IntObj, Relation, IntObj) -> PDDL
createPDDL objects ((Simply o1), r, IFloor) = PDDL r id1 ""
    where id1 = fst $ head $ filter ((== o1) . snd) objects
createPDDL objects ((Simply o1), r, (Simply o2)) = PDDL r id1 id2
  where id1 = fst $ head $ filter ((== o1) . snd) objects
        id2 = fst $ head $ filter ((== o2) . snd) objects
createPDDL objects (IFloor, _, _) = error "panic! the impossible happened!"
{-
World :: [[Id]]
Id :: String
(Int, Int) is which list and position in the list that the given Id is in
-}
getIndices :: World -> Id -> Int -> (Int, Int)
getIndices [] id _ = error $ "Can't find the element with id " ++ show id
getIndices (ids:idss) id x
  | id `elem` ids = (x, fromJust (elemIndex id ids))
  | otherwise     = getIndices idss id (x + 1)
