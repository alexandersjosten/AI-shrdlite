module Interpreter where

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Text.JSON.Types
import Data.List (findIndex)
--import Text.Groom
import Data.Maybe

import HelpFunctions hiding (Floor, Box, Ball)

{-
  Nasty way, lookup table way better! :)
  a = fromJSObject objects :: [(String, JSValue)]
  b = map snd a            :: [JSValue]
  c = map encode b         :: [String]
  Use Command to build a JSON string and then do:
  map (isInfixOf cmd) c    :: [Bool]
-}
interpret :: World -> Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = [True]

interpret' :: Command -> [PDDL]
interpret' tree =
  case translateCommand tree of
    (Just o1, Just o2) ->
      case findIndex ((== o1) . snd) listOfObjects of
        Just i1 -> case findIndex ((== o2) . snd) listOfObjects of
          Just i2 -> createPDDL (i1, i2)
          Nothing -> error $ "Can't find object " ++ show o2
        Nothing -> error $ "Can't find object " ++ show o1
    (Just o, Nothing)  -> undefined
    (_, _)             -> error "Don't know what to do, can't move nothing!"

-- Translates the command and returns JSON-strings
translateCommand :: Command -> (Maybe Object, Maybe Object)
translateCommand (Move e l) = (translateEntity e, translateLocation l)
translateCommand (Take e)   = (translateEntity e, Nothing)
translateCommand (Put l)    = (Nothing, translateLocation l)

-- TODO: RelativeEntity location, check where in world object is located
translateEntity :: Entity -> Maybe Object
translateEntity Floor                  = Nothing
translateEntity (BasicEntity _ o)      = Just o
translateEntity (RelativeEntity _ o _) = Just o

-- TODO: Relation of the location
translateLocation :: Location -> Maybe Object
translateLocation (Relative _ e) = translateEntity e

createPDDL :: (Int, Int) -> [PDDL]
createPDDL (i1, i2) = [PDDL Ontop (fst (listOfObjects !! i1)) (fst (listOfObjects !! i2))]
