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
