module Interpreter (
  -- Only export needed functions, rest are help functions
  interpret
) where

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Text.JSON.Types
import Data.List (elemIndex)
import Data.List
import Data.Maybe
import HelpFunctions

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

-- RelativeEntity Quantifier Object Location
translateCmd :: Command -> [(Id, Object)] -> World -> Id -> [[(Id, Relation, Id)]]
translateCmd cmd os w holding =
  case cmd of
    Move Floor _ -> error "translateCommand: Can't move floor!"
    Move e     l -> createTriple (getEntities e os w) (getLocations l os w) os
    Take Floor   -> error "translateCommand: Can't take floor!"
    Take e       -> createTriple (getEntities e os w) [(Ontop, "")] os
    Put        l -> case holding of
      "" -> error "translateCommand: Can't put anything down, not holding anything!"
      id -> createTriple (The, [id]) (getLocations l os w) os

createTriple :: (Quantifier, [Id]) -> [(Relation, Id)] -> [(Id, Object)] -> [[(Id, Relation, Id)]]
createTriple (_, [] ) _  _            = []
createTriple (q, ids'@(id:ids)) rs os = case q of
  All -> [createAllTriple' ids' rs os]
  Any -> [head $ createTriple' id rs : createTriple (q, ids) rs os]
  The -> createTriple' id rs : createTriple (q, ids) rs os
  where createAllTriple' :: [Id] -> [(Relation, Id)] -> [(Id, Object)] -> [(Id, Relation, Id)]
        createAllTriple' []       _  _  = []
        createAllTriple' _        [] _  = []
        createAllTriple' (id:ids) rs os = findLegal id rs os : createAllTriple' ids rs os

        createTriple' :: Id -> [(Relation, Id)] -> [(Id, Relation, Id)]
        createTriple' _  []            = []
        createTriple' id ((r, id'):rs) = (id, r, id') : createTriple' id rs

findLegal :: Id -> [(Relation, Id)] -> [(Id, Object)] -> (Id, Relation, Id)
findLegal id  []          _  = error $ "findLegal: Can't find legal combination!"
findLegal id ((r, i):rs) os =
  if i == "floor" || okMove o1 o2 then
    (id, r, i)
  else
    findLegal id rs os
  where o1 = snd $ head $ filter ((== id) . fst) os
        o2 = snd $ head $ filter ((== i ) . fst) os
  

getEntities :: Entity -> [(Id, Object)] -> World -> (Quantifier, [Id])
getEntities Floor                               _  _ = (The, ["floor"])
getEntities (BasicEntity q o)                   os _ =
  case getObjectIds o os of
    [] -> (q, []) --error $ "getEntities 1: Can't find any objects matching " ++ show o
    xs -> case q of
      Any -> (q, [head xs])
      _   -> (q, xs)
getEntities (RelativeEntity q o l@(Relative r e)) os w =
  case getObjectIds o os of
    [] -> (q, []) --error $ "getEntities 2: Can't find any objects matching " ++ show o
    xs -> case getLocations l os w of
      [] -> (q, []) --error $ "getEntities 3: Can't find any entities matching " ++ show e
      ys -> case checkRelations xs ys w os of
        [] -> (q, []) --error $ "getEntities 4: Can't find any entities matching " ++ show e ++ " " ++ show r ++ " " ++ show ys
        zs -> case q of
          Any -> (q, [head zs])
          _   -> (q, zs)

getLocations :: Location -> [(Id, Object)] -> World -> [(Relation, Id)]
getLocations (Relative r e) os w = [(r, id) | id <- id']
  where (_, id') = getEntities e os w

checkWorld :: Id -> World -> [(Id, Object)] -> (Relation, Id) -> Bool
checkWorld id1 w objects (r, id2)
  | id1 /= id2 && id1 /= "floor" && id2 /= "floor" =
      case r of
        Beside  -> i1 /= i2
        Leftof  -> i1 < i2
        Rightof -> i1 > i2
        Above   -> i1 == i2 && i1' < i2'
        Ontop   -> i1 == i2 && i1' == (i2' - 1)
        Under   -> i1 == i2 && i1' > i2'
        Inside  -> i1 == i2 && i1' == (i2' - 1) && getForm id2 == Box
  | otherwise = False
  where getForm :: Id -> Form
        getForm id = (\(Object _ _ f) -> f) (snd $ head $ filter ((== id) . fst) objects)
        (i1, i1')  = getIndices w id1
        (i2, i2')  = getIndices w id2

checkRelations :: [Id] -> [(Relation, Id)] -> World -> [(Id, Object)] -> [Id]
checkRelations []       _  _ _  = []
checkRelations (id:ids) ys w os =
  case filter (checkWorld id w os) ys of
    [] -> checkRelations ids ys w os
    _  -> id : checkRelations ids ys w os

-- Get all the ids matching a given object that exist in the current world
getObjectIds :: Object -> [(Id, Object)] -> [Id]
getObjectIds (Object AnySize AnyColor AnyForm) = map fst
getObjectIds (Object AnySize AnyColor f      ) = map fst . filter (\(_, Object _  _  f') -> f == f')
getObjectIds (Object AnySize c        AnyForm) = map fst . filter (\(_, Object _  c' _ ) -> c == c')
getObjectIds (Object s       AnyColor AnyForm) = map fst . filter (\(_, Object s' _  _ ) -> s == s')
getObjectIds (Object AnySize c        f      ) = map fst . filter (\(_, Object _  c' f') -> c == c' && f == f')
getObjectIds (Object s       AnyColor f      ) = map fst . filter (\(_, Object s' _  f') -> s == s' && f == f')
getObjectIds (Object s       c        AnyForm) = map fst . filter (\(_, Object s' c' _ ) -> s == s' && c == c')
getObjectIds (Object s       c        f      ) = map fst . filter (\(_, Object s' c' f') -> s == s' && c == c' && f == f')

-- Get the exact position of a given Id in the current World
getIndices :: World -> Id -> (Int, Int)
getIndices w id = getIndices' w id 0
  where getIndices' :: World -> Id -> Int -> (Int, Int)
        getIndices' []     id _ = error $ "getIndices: Can't find the element with id " ++ show id
        getIndices' (w:ws) id x
          | id `elem` w = (x, fromJust (elemIndex id w))
          | otherwise   = getIndices' ws id (x + 1)
