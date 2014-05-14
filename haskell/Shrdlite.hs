#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)
import AmbigRes
import HelpFunctions -- and types
import Interpreter
import Planner
import Data.Char

main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode

--Test Goals

--Put the white ball(e) in the large yellow box(k) beside the large red box(l) beside the large yellow pyramid(i)
testGoals1 = [PDDL Ontop "e" "k", PDDL Beside "e" "l", PDDL Beside "l" "i"]
-- Put the black ball in the small blue box ontop of the large yellow box beside the large green brick
testGoals2 = [PDDL Ontop "f" "m", PDDL Beside "m" "a", PDDL Ontop "m" "k"]

jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput =
  
  case amb of
    Error a -> makeObj result
    Ok prev -> makeObj clarResult
    
    where
      (wasAmbig, ambList, theChoices) =
        case resolveAmbig goals of
          Right _ -> (False, showJSON "",[""])
          Left x -> (True, showJSON $ show goals, buildChoices x)
          
      utterance = ok (valFromObj "utterance" jsinput) :: Utterance
      
      world = ok (valFromObj "world" jsinput) :: World

      holding = case valFromObj "holding" jsinput of
        Ok id -> id
        Error _ -> ""

      hold = ok (valFromObj "hold" jsinput) :: Id
      objects = ok (valFromObj "objects" jsinput) :: Objects

      amb = fmap read (valFromObj "amb" jsinput) :: Result [[PDDL]]

      trees = parse command utterance :: [Command]

      goals = filter (/= []) [goal | tree <- trees, goal <- interpret world holding objects tree] :: [[PDDL]]

      ambGoals = case getChoice utterance of
        Nothing -> error "Can't happen!"
        Just x -> filterChoice x goals

      plan = --"kisi"
        case goals of
             [] -> error "Fucking interpreter couldn't find a goal!"
             ([]:g:gs) -> solve world hold holding g :: Plan
             (g:gs) -> solve world hold holding g :: Plan

      output
        | null trees = ["Parse error!"]
        | null goals = ["Interpretation error!"]
        | wasAmbig = theChoices
        | null plan = ["Planning error!"]
        | otherwise = ["Success!"]


      ambOutput = case getChoice utterance of
        Nothing -> ["I told you to give me a choice of 1-5?! Restarting!"]
        Just x -> ["Nice choice of " ++ [intToDigit x]]
    
      clarified = case getChoice utterance of
        Nothing -> False
        Just x -> True
                   
      result = [("utterance", showJSON utterance),
                   ("trees", showJSON (map show trees)),
                   ("goals", if length trees >= 1 then showJSON (map show goals) else JSNull),
                   ("plan", if length goals == 1 then showJSON plan else JSNull),
                   ("output", showJSON output)
                  ] ++ if wasAmbig then [("amb", ambList)] else []
                                                                         
      clarResult = [("utterance",showJSON utterance),--TODO Filter out ambiguity here
                  -- ("trees", showJSON (map show trees)),
                   ("goals", if clarified then showJSON (map show ambGoals) else JSNull),
                   ("plan", if length goals == 1 then showJSON plan else JSNull),
                   ("output", showJSON ambOutput)
                  ] ++ if wasAmbig then [("amb", ambList)] else []

getChoice :: [String] -> Maybe Int
getChoice [] = Nothing
getChoice [[x]]
  | isDigit x
    && digitToInt x >= 1
    && digitToInt x < 6 = Just (digitToInt x)
  | otherwise = Nothing

filterChoice :: Int -> PDDLWorld -> [PDDL]
filterChoice x yss =
  case resolveAmbig yss of
    Right b -> b
    Left a -> filterGoal (atype, id, list !! (x-1)) yss
      where (Ambiguity atype id list) = a


filterGoal :: (AmbType, Id, Id) -> PDDLWorld -> [PDDL]
filterGoal (t,i1,i2) (x:xs)
  | t == Source =
    let ys = [(PDDL rel id1 id2) | (PDDL rel id1 id2) <- x,i1 == id1 && i2 == id2 ]
      in if null ys
         then filterGoal (t,i1,i2) xs
         else x
  | otherwise =
    let ys = [(PDDL rel id1 id2) | (PDDL rel id1 id2) <- x,i2 == id1 && i1 == id2 ]
      in if null ys
         then filterGoal (t,i1,i2) xs
         else x
