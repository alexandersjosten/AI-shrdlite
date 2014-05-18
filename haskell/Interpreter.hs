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
import HelpFunctions hiding (convertWorld)
import Control.Monad

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

testInterpret :: World -> Id -> [(Id, Object)] -> Command -> [[PDDL]]
testInterpret world holding objects cmd =
  case holding of
    "" ->
      case pickBest world allPDDLs of
        [] -> case (q1, q1') of
          (The, The) -> allPDDLs
          _   -> take 1 allPDDLs
        xs -> case (q1, q1') of
          (The, The) -> allPDDLs
          _   -> take 1 xs --nub $ createPDDL $ translateCommand tree os world holding
    id ->
      case pickBest world allPDDLs' of
        [] -> case (q2, q2') of
          (The, The) -> allPDDLs'
          _   -> take 1 allPDDLs'
        xs -> case (q2, q2') of
          (The, The) -> allPDDLs'
          _   -> take 1 xs
    where os              = filterWorld world objects
          adding          = ((filter ((== holding) . fst) objects) ++ os)
          
          (q1, q1', ps)   = translateCommand cmd os world holding
          allPDDLs        = nub $ createPDDL ps
          
          (q2, q2', ps') = translateCommand cmd adding world holding
          allPDDLs'       = nub $ createPDDL ps'

-- Interpret function to be exported. Will start the entire interpretation process
interpret :: World -> Id -> Objects -> Command -> [[PDDL]]
interpret world holding objects tree =
  case holding of
    "" ->
      case pickBest world allPDDLs of
        [] -> case (q1, q1') of
          (The, The) -> allPDDLs
          _   -> take 1 allPDDLs
        xs -> case (q1, q1') of
          (The, The) -> allPDDLs
          _   -> take 1 xs --nub $ createPDDL $ translateCommand tree os world holding
    id ->
      case pickBest world allPDDLs' of
        [] -> case (q2, q2') of
          (The, The) -> allPDDLs'
          _   -> take 1 allPDDLs'
        xs -> case (q2, q2') of
          (The, The) -> allPDDLs'
          _   -> take 1 xs
    where table           = createTable objects
          os              = filterWorld world table
          adding          = ((filter ((== holding) . fst) table) ++ os)
          
          (q1, q1', ps)   = translateCommand tree os world holding
          allPDDLs        = nub $ createPDDL ps
          
          (q2, q2', ps') = translateCommand tree adding world holding
          allPDDLs'       = nub $ createPDDL ps'

-- Function to create the actual PDDLs given how the command to the interpreter
-- is interpreted
createPDDL :: [[(Id, Relation, Id)]] -> [[PDDL]]
createPDDL xs = map createPDDL' xs
  where createPDDL' :: [(Id, Relation, Id)] -> [PDDL]
        createPDDL' [] = []
        createPDDL' ((id1, r, id2):pddls) = PDDL r id1 id2 : createPDDL' pddls

pickBest :: World -> [[PDDL]] -> [[PDDL]]
pickBest w ts = pickBest' (convertWorld w) ts 0 []

pickBest' :: PDDLWorld -> [[PDDL]] -> Int -> [[PDDL]] -> [[PDDL]]
pickBest' _ []     _ res = res
pickBest' w (t:ts) x res =
  if checkCount > x then
    pickBest' w ts checkCount [t]
  else
    pickBest' w ts x res
  where checkCount = countSatisfied (concat w) t

countSatisfied :: [PDDL] -> [PDDL] -> Int
countSatisfied w ps = countSatisfied' w ps 0
  where countSatisfied' ::[PDDL] -> [PDDL] -> Int -> Int
        countSatisfied' _ []     count = count
        countSatisfied' w (p:ps) count
          | p `elem` w = countSatisfied' w ps (count + 1)
          | otherwise  = countSatisfied' w ps count

-- Convert world from given form to PDDL form
convertWorld :: World -> PDDLWorld
convertWorld []     = []
convertWorld (c:cs) = (createPDDLWorld c) : convertWorld  cs
  where createPDDLWorld []     = []
        createPDDLWorld (x:xs) =
          PDDL Ontop x ("floor") : [ getPDDL (c !! (i+1)) (c !! i)
                                   | i <-[0..length xs -1]
                                   ]

getPDDL :: Id -> Id -> PDDL
getPDDL id1 id2 =
  if o2 == Box && o1 == Ball then
    PDDL Inside id1 id2
  else
    PDDL Ontop id1 id2
  where (Object _ _ o2) = getObjId id2
        (Object _ _ o1) = getObjId id1

-- Filters all the objects from the JSON-part that doesn't exist in the world
filterWorld :: World -> [(Id, Object)] -> [(Id, Object)]
filterWorld world mapping = filter ((`elem` flatWorld) . fst) mapping
  where flatWorld = concat world
        
--------------------------------------------------------------------------------
--------------------------------- Lookup table ---------------------------------
--------------------------------------------------------------------------------
-- creates a lookup table over the objects given a JSON blob of data
createTable :: Objects -> [(Id, Object)]
createTable os = createTable' (ids `zip` os')
  where a = fromJSObject os
        ids = map fst a
        os' = map encode $ map snd a

createTable' :: [(Id, String)] -> [(Id, Object)]
createTable' [] = []
createTable' ((id, s):xs) =
  (id, Object getSize getColor getForm) : createTable' xs
  where getSize = findStuff sizeTable s
        getColor = findStuff colorTable s
        getForm = findStuff formTable s

-- We should always find something..
findStuff :: [(String, a)] -> String -> a
findStuff [] s = error $ "Couldn't match the string " ++ show s ++ " to something"
findStuff ((id, obj):xs) s
  | id `isInfixOf` s = obj
  | otherwise = findStuff xs s

--------------------------------------------------------------------------------

-- Function that will initiate the translation of a given command (i.e. a parse tree)
-- in a bottom-up fashion
translateCommand :: Command -> [(Id, Object)] -> World -> Id -> (Quantifier, Quantifier, [[(Id, Relation, Id)]])
translateCommand cmd os w holding =
  case cmd of
    Move Floor _ -> error "translateCommand: Can't move floor!"
    Move e     l -> (q, q', createTriples es ls r (q, q') os w)
      where (q, es)     = getEntities e os w
            (q', r, ls) = getLocations l os w
    Take Floor   -> error "translateCommand: Can't take floor!"
    Take e       -> (q, Any, createTriples es [""] Ontop (q, Any) os w)
      where (q, es) = getEntities e os w
    Put l        ->
      case holding of
        "" -> error "translateCommand: Can't put anything down, not holding anything!"
        id -> (The, q, createTriples [id] ls r (The, q) os w)
          where (q, r,ls) = getLocations l os w

-- Get all the IDs that match the given entity in the current world.
-- Also saves the quantifier for later
getEntities :: Entity -> [(Id, Object)] -> World -> (Quantifier, [Id])
getEntities Floor _ _ = (The, ["floor"])
getEntities (BasicEntity q o) os _ =
  case getObjectIds o os of
    [] -> error $ "getEntities: can't find the object " ++ show o --(q, [])
    xs -> (q, xs)
getEntities (RelativeEntity q o l@(Relative r e)) os w =
  case getObjectIds o os of
    [] -> error $ "getEntities: can't find the object " ++ show o --(q, [])
    xs -> case getLocations l os w of
      (_, _, []) -> error $ "getEntities : Can't find any entities matching " ++ show e
      (q', r', ys) -> case checkRelations xs ys r' q' w os of
        [] -> (q, [])
        zs -> (q, zs)

-- Gets all the IDs of objects that match the given location in the current world.
-- Keeps track of the quantifier as well as the relation an entity should have to
-- the location entities
getLocations :: Location -> [(Id, Object)] -> World -> (Quantifier, Relation, [Id])
getLocations (Relative r e) os w =
  case e of
    Floor -> (The, r, ["floor"])
    BasicEntity q _ -> (q, r, [id | id <- id'])
    RelativeEntity q _ _ -> (q, r, [id | id <- id'])
  where (_, id') = getEntities e os w

-- Creates a triple of data to be used when creating the PDDLs
createTriples :: [Id] -> [Id] -> Relation -> (Quantifier, Quantifier) -> [(Id, Object)] -> World -> [[(Id, Relation, Id)]]
createTriples []            _         _ _  _  _ = []
createTriples ids           [""]      r qs os w =
  case qs of
    (Any, Any) -> take 1 [[(id, r, "")] | id <- ids]
    _          -> [[(id, r, "")] | id <- ids]
createTriples ids           ["floor"] r qs os w =
  case qs of
    (Any, The) -> take 1 $ [[(id, r, "floor")] | id <- ids]
    (All, The) -> [[(id, r, "floor") | id <- ids]]
    (The, The) -> [[(id, r, "floor")] | id <- ids]
createTriples ids'@(id:ids) ls        r qs os w =
  case qs of
    (All, Any) ->
      makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids'
    (The, Any) ->
      if length ids' > 1 then -- We have ambiguity...
        concat $ map (map (:[])) (take 1 $ makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids')
      else
        makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids'
    (The, The) -> makeValid $ concat $ map (createTriples' ls r os) ids'
    (Any, Any) -> makeValid $ concat $ map (createTriples' ls r os) ids'
    (Any, The) -> concat $ map (createTriples' ls r os) ids'
    (Any, All) -> makeValidAnyAll $ merge (length ls) $ concat $ map (createTriples' ls r os) ids'
    (The, All) -> makeValid $ merge (length ls) $ concat $ map (createTriples' ls r os) ids'
    (All, All) -> makeValid $ merge (length ls) $ concat $ map (createTriples' ls r os) ids'
    _          -> error "I can't do that, Dave... Don't know what you mean!"

-- Help function, will create all triples to the locations given an source ID.
-- Will only save legal combinations
createTriples' :: [Id] -> Relation -> [(Id, Object)] -> Id -> [[(Id, Relation, Id)]]
createTriples' []     _ _  _  = []
createTriples' (l:ls) r os id =
  if r == Inside || r == Ontop then
    if isLegal id l os then
      [(id, r, l)] : createTriples' ls r os id
    else
      createTriples' ls r os id
  else
    if r == Under && isLegal l id os then
      [(id, r, l)] : createTriples' ls r os id
    else
      createTriples' ls r os id
      
-- Function to create the different combinations of elements which depends
-- on the quantifiers in createTriples.
merge :: Int -> [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
merge x xs = concat $ replicateM x xs

-- Takes a list (from merge) and removes every set of combinations that isn't valid,
-- e.g. two elements to the same source in the same sublist is not valid
makeValid :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
makeValid xs = makeValid' xs []
  where makeValid' :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
        makeValid' []     res = nub $ map sort res
        makeValid' (x:xs) res = if valid x then
                                  makeValid' xs (x:res)
                                else
                                  makeValid' xs res

-- Function to check if a sublist is valid or not
valid :: [(Id, Relation, Id)] -> Bool
valid []                     = True
valid ((_, _, "floor"):rest) = valid rest
valid (x:xs)
  | l1 < lRest = False
  | l2 < lRest = False
  | otherwise  = valid xs
  where l1    = length (filter ((/= first  x) . first ) xs)
        l2    = length (filter ((/= second x) . second) xs)
        lRest = length xs
        
        first  :: (Id, Relation, Id) -> Id
        first  (id, _, _) = id

        second :: (Id, Relation, Id) -> Id
        second (_, _, id) = id


makeValidAnyAll :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
makeValidAnyAll xs = makeValidAnyAll' xs []

makeValidAnyAll' :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
makeValidAnyAll' []     res = nub $ map sort res
makeValidAnyAll' (x:xs) res = if valid' x then
                                makeValidAnyAll' xs (x:res)
                              else
                                makeValidAnyAll' xs res

valid' :: [(Id, Relation, Id)] -> Bool
valid' []     = True
valid' (x:xs)
  | l1 == lRest && l2 == lRest = True
  | otherwise   = False
  where l1    = length (filter ((== first  x) . first ) xs)
        l2    = length (filter ((/= second x) . second) xs)
        lRest = length xs
        
        first  :: (Id, Relation, Id) -> Id
        first  (id, _, _) = id

        second :: (Id, Relation, Id) -> Id
        second (_, _, id) = id
        
-- Checks if it is legal to perform a move between two given IDs. Function is
-- only called if the relation between the two entities will be either ontop or
-- inside. It is legal if the destination is either the floor or if it's allowed to
-- move the object corresponding to id1 to the object corresponding to id2
isLegal :: Id -> Id -> [(Id, Object)] -> Bool
isLegal id1 id2 os = id2 == "floor" || okMove o1 o2
  where o1 = snd $ head $ filter ((== id1) . fst) os
        o2 = snd $ head $ filter ((== id2) . fst) os

-- Function to check a given relation between a list of source entities and a list of
-- destionation entities
checkRelations :: [Id] -> [Id] -> Relation -> Quantifier -> World -> [(Id, Object)] -> [Id]
checkRelations [] _ _ _ _ _ = []
checkRelations (id:ids) ys r q w os =
  if checkWorld id ys r q w os then
    id : checkRelations ids ys r q w os
  else
    checkRelations ids ys r q w os

-- Checks the different destinations against a specific source
checkWorld :: Id -> [Id] -> Relation -> Quantifier -> World -> [(Id, Object)] -> Bool
checkWorld id ids r q w objects
  | and (map (/= id) ids) && id /= "floor" && and (map (/= "floor") ids) =
      case r of
        Beside  -> checkWorld' q (map ((/= i1) . fst) indices)
        Leftof  -> checkWorld' q (map ((i1 <) . fst) indices)
        Rightof -> checkWorld' q (map ((i1 >) . fst) indices)
        Above   -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' > i2') indices)
        Ontop   -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' == (i2' + 1)) indices)
        Under   -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' < i2') indices)
        Inside  -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' == (i2' + 1)) indices) --getForm id2 == Box
  | and (map (== "floor") ids) =
      case r of
        Ontop -> i1' == 0
        _ -> False
  | otherwise = False
  where getForm :: Id -> Form
        getForm id = (\(Object _ _ f) -> f) (snd $ head $ filter ((== id) . fst) objects)
        
        (i1, i1') = getIndices w id
        indices = map (getIndices w) ids

        checkWorld' :: Quantifier -> [Bool] -> Bool
        checkWorld' All xs = and xs
        checkWorld' _   xs = or xs


-- Get all the ids matching a given object that exist in the current world
getObjectIds :: Object -> [(Id, Object)] -> [Id]
getObjectIds (Object AnySize AnyColor AnyForm) = map fst
getObjectIds (Object AnySize AnyColor f ) = map fst . filter (\(_, Object _ _ f') -> f == f')
getObjectIds (Object AnySize c AnyForm) = map fst . filter (\(_, Object _ c' _ ) -> c == c')
getObjectIds (Object s AnyColor AnyForm) = map fst . filter (\(_, Object s' _ _ ) -> s == s')
getObjectIds (Object AnySize c f ) = map fst . filter (\(_, Object _ c' f') -> c == c' && f == f')
getObjectIds (Object s AnyColor f ) = map fst . filter (\(_, Object s' _ f') -> s == s' && f == f')
getObjectIds (Object s c AnyForm) = map fst . filter (\(_, Object s' c' _ ) -> s == s' && c == c')
getObjectIds (Object s c f ) = map fst . filter (\(_, Object s' c' f') -> s == s' && c == c' && f == f')

-- Get the exact position of a given Id in the current World
getIndices :: World -> Id -> (Int, Int)
getIndices w id = getIndices' w id 0
  where getIndices' :: World -> Id -> Int -> (Int, Int)
        getIndices' [] id _ = error $ "getIndices: Can't find the element with id " ++ show id
        getIndices' (w:ws) id x
          | id `elem` w = (x, fromJust (elemIndex id w))
          | otherwise = getIndices' ws id (x + 1)
