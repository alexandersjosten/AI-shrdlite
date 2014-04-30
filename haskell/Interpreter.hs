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
    where os = createTable objects
--------------------------------------------------------------------------------
--------------------------------- Lookup table ---------------------------------
--------------------------------------------------------------------------------
createTable :: Objects -> [(Id, Object)]
createTable os = error $ show $ createTable' (ids `zip` os')
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
-- Will use Maybe in order to make it easier
translateCommand :: Command -> [(Maybe Object, (Relation, Maybe Object))]
translateCommand cmd =
  case cmd of
    Move Floor _ -> error "Can't move the floor!"
    Move e     l -> createPairs (getEntities e) (getLocations l)
    Take Floor   -> error "Can't take the floor!"
    Take e       -> createPairs (getEntities e) [(Ontop, Nothing)]
    Put        l -> createPairs [Nothing] (getLocations l)

createPairs :: [Maybe Object] -> [(Relation, Maybe Object)] -> [(Maybe Object, (Relation, Maybe Object))]
createPairs o1 o2 = [(o1', o2') | o1' <- o1, o2' <- o2]


-- If we get a quantifier saying Any, just pick the first one
getEntities :: Entity -> [Maybe Object]
getEntities (BasicEntity q o) =
  case q of
    The -> translateObject o listOfObjects
    Any -> [head $ translateObject o listOfObjects]
    All -> translateObject o listOfObjects
getEntities (RelativeEntity q o l) =
  case q of
    The -> undefined
    Any -> undefined
    All -> undefined

translateObject :: Object -> [(Id, Object)] -> [Maybe Object]
translateObject (Object AnySize AnyColor f) = map Just . filter (\(Object _  _  f') -> f == f') . map snd
translateObject (Object AnySize c        f) = map Just . filter (\(Object _  c' f') -> c == c' && f == f') . map snd
translateObject (Object s       AnyColor f) = map Just . filter (\(Object s' _  f') -> s == s' && f == f') . map snd
translateObject (Object s       c        f) = map Just . filter (\(Object s' c' f') -> s == s' && c == c' && f == f') . map snd

-- Relative Relation Entity
getLocations :: Location -> [(Relation, Maybe Object)]
getLocations (Relative r e) = [(r, e')| e' <- getEntities e]

-- Beside | Leftof | Rightof | Above | Ontop | Under | Inside
{-checkLocation :: World -> Relation -> [Maybe Object] -> [Maybe Object]
checkLocation world r es =
  case r of
    Beside  -> undefined
    Leftof  -> undefined
    Rightof -> undefined
    Above   -> undefined
    Ontop   -> undefined
    Under   -> undefined
    Inside  -> undefined

createPDDL :: (Int, Int) -> [PDDL]
createPDDL (i1, i2) = [PDDL Ontop (fst (listOfObjects !! i1)) (fst (listOfObjects !! i2))]
-}

createPDDL :: (Maybe Object, (Relation, Maybe Object)) -> PDDL
createPDDL (o1, (r, Nothing)) = PDDL r id1 ""
    where id1 = fst $ head $ filter ((== o1) . snd) (map (\(id, o) -> (id, Just o)) listOfObjects)
createPDDL (o1, (r, o2)) = PDDL r id1 id2
  where id1 = fst $ head $ filter ((== o1) . snd) (map (\(id, o) -> (id, Just o)) listOfObjects)
        id2 = fst $ head $ filter ((== o2) . snd) (map (\(id, o) -> (id, Just o)) listOfObjects)
