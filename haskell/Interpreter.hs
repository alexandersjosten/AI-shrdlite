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
    "" -> nub $ createPDDL $ translateCmd cmd os world holding
    id -> nub $ createPDDL $ translateCmd cmd adding world holding
    where os = filterWorld world objects
          adding = ((filter ((== holding) . fst) objects) ++ os)

createPDDL :: [[(Id, Relation, Id)]] -> [[PDDL]]
createPDDL xs = map createPDDL' xs
  where createPDDL' :: [(Id, Relation, Id)] -> [PDDL]
        createPDDL' [] = []
        createPDDL' ((id1, r, id2):pddls) = PDDL r id1 id2 : createPDDL' pddls

interpret :: World -> Id -> Objects -> Command -> [[PDDL]]
interpret world holding objects tree =
  case holding of
    "" -> nub $ createPDDL $ translateCmd tree os world holding
    id -> nub $ createPDDL $ translateCmd tree adding world holding
    where table = createTable objects
          os = filterWorld world table
          adding = ((filter ((== holding) . fst) table) ++ os)

-- Filters all the objects from the JSON-part that doesn't exist in the world
filterWorld :: World -> [(Id, Object)] -> [(Id, Object)]
filterWorld world mapping = filter ((`elem` flatWorld) . fst) mapping
  where flatWorld = concat world
--------------------------------------------------------------------------------
--------------------------------- Lookup table ---------------------------------
--------------------------------------------------------------------------------
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

getLocations :: Location -> [(Id, Object)] -> World -> (Quantifier, Relation, [Id])
getLocations (Relative r e) os w =
  case e of
    Floor -> (The, r, ["floor"])
    BasicEntity q _ -> (q, r, [id | id <- id'])
    RelativeEntity q _ _ -> (q, r, [id | id <- id'])
  where (_, id') = getEntities e os w

translateCmd :: Command -> [(Id, Object)] -> World -> Id -> [[(Id, Relation, Id)]]
translateCmd cmd os w holding =
  case cmd of
    Move Floor _ -> error "translateCommand: Can't move floor!"
    Move e l -> createTriples es ls r (q, q') os
      where (q', r, ls) = getLocations l os w
            (q, es) = getEntities e os w
    Take Floor -> error "translateCommand: Can't take floor!"
    Take e -> createTriples es [""] Ontop (q, Any) os
      where (q, es) = getEntities e os w
    Put l -> case holding of
      "" -> error "translateCommand: Can't put anything down, not holding anything!"
      id -> createTriples [id] ls r (The, q) os
        where (q, r,ls) = getLocations l os w


createTriples :: [Id] -> [Id] -> Relation -> (Quantifier, Quantifier) -> [(Id, Object)] -> [[(Id, Relation, Id)]]
createTriples []            _         _ _  _  = []
createTriples ids           [""]      r qs os = [[(id, r, "")] | id <- ids]
createTriples ids           ["floor"] r qs os =
  case qs of
    (Any, The) -> take 1 $ [[(id, r, "floor")] | id <- ids]
    (All, The) -> [[(id, r, "floor") | id <- ids]]
    (The, The) -> [[(id, r, "floor")] | id <- ids]
createTriples ids'@(id:ids) ls        r qs os =
  case qs of
    (All, Any) ->
      if length ids' > length ls then
        error "C'mon man, I don't have enough locations for you!"
      else
        take 1 $ makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids'
    (The, Any) ->
      if length ids' > 1 then -- We have ambiguity...
        concat $ map (map (:[])) (take 1 $ makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids')
      else
        take 1 $ makeValid $ merge (length ids') $ concat $ map (createTriples' ls r os) ids'
    (The, The) -> makeValid $ merge 1 $ concat $ map (createTriples' ls r os) ids'
    (Any, Any) -> take 1 $ makeValid $ merge 1 $ concat $ map (createTriples' ls r os) ids'
    (Any, The) -> take 1 $ makeValid $ merge (length ls) $ concat $ map (createTriples' ls r os) ids'
    _          -> error "I can't do that, Dave... Don't know what you mean!"

createTriples' :: [Id] -> Relation -> [(Id, Object)] -> Id -> [[(Id, Relation, Id)]]
createTriples' []     _ _  _  = []
createTriples' (l:ls) r os id =
  if r == Inside || r == Ontop then
    if isLegal id l r os then
      [(id, r, l)] : createTriples' ls r os id
    else
      createTriples' ls r os id
  else [(id, r, l)] : createTriples' ls r os id

merge :: Int -> [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
merge x xs =
  case x of
    1 -> xs
    2 -> [trip1 ++ trip2 | trip1 <- xs, trip2 <- xs]
    3 -> [trip1 ++ trip2 ++ trip3 | trip1 <- xs, trip2 <- xs, trip3 <- xs]
    4 -> [trip1 ++ trip2 ++ trip3 ++ trip4 | trip1 <- xs, trip2 <- xs, trip3 <- xs, trip4 <- xs]
    5 -> [trip1 ++ trip2 ++ trip3 ++ trip4 ++ trip5 | trip1 <- xs, trip2 <- xs, trip3 <- xs, trip4 <- xs, trip5 <- xs]

makeValid :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
makeValid xs = makeValid' xs []
  where makeValid' :: [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]] -> [[(Id, Relation, Id)]]
        makeValid' []     res = nub $ map sort res
        makeValid' (x:xs) res = if valid x then
                                  makeValid' xs (x:res)
                                else
                                  makeValid' xs res

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

isLegal :: Id -> Id -> Relation -> [(Id, Object)] -> Bool
isLegal id1 id2 r os = id2 == "floor" || okMove o1 o2
  where o1 = snd $ head $ filter ((== id1) . fst) os
        o2 = snd $ head $ filter ((== id2) . fst) os

findLegal :: Id -> [Id] -> Relation -> [(Id, Object)] -> (Id, Relation, Id)
findLegal id [] r _ = ("", r, "")
findLegal id (id':ids) r os =
  if id' == "floor" || okMove o1 o2 then
    (id, r, id')
  else
    findLegal id ids r os
  where o1 = snd $ head $ filter ((== id ) . fst) os
        o2 = snd $ head $ filter ((== id') . fst) os

checkWorld :: Id -> [Id] -> Relation -> Quantifier -> World -> [(Id, Object)] -> Bool
checkWorld id ids r q w objects
  | and (map (/= id) ids) && id /= "floor" && and (map (/= "floor") ids) =
      case r of
        Beside -> checkWorld' q (map ((/= i1) . fst) indices)
        Leftof -> checkWorld' q (map ((i1 <) . fst) indices)
        Rightof -> checkWorld' q (map ((i1 >) . fst) indices)
        Above -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' < i2') indices)
        Ontop -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' == (i2' + 1)) indices)
        Under -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' < i2') indices)
        Inside -> checkWorld' q (map (\(i2, i2') -> i1 == i2 && i1' == (i2' + 1)) indices) --getForm id2 == Box
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
        checkWorld' _ xs = or xs

checkRelations :: [Id] -> [Id] -> Relation -> Quantifier -> World -> [(Id, Object)] -> [Id]
checkRelations [] _ _ _ _ _ = []
checkRelations (id:ids) ys r q w os =
  if checkWorld id ys r q w os then
    id : checkRelations ids ys r q w os
  else
    checkRelations ids ys r q w os

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
