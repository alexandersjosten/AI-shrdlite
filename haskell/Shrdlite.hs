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
main = do
  input <- getContents
  putStrLn . encode . jsonMain . ok $  decode input

--Test Goals

--Put the white ball(e) in the large yellow box(k) beside the large red box(l) beside the large yellow pyramid(i)
testGoals1 = [PDDL Ontop "e" "k", PDDL Beside "e" "l", PDDL Beside "l" "i"]
-- Put the black ball in the small blue box ontop of the large yellow box beside the large green brick
testGoals2 = [PDDL Ontop "f" "m", PDDL Beside "m" "a", PDDL Ontop "m" "k"]

jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput =
  let amb = (valFromObj "state" jsinput) :: Result String in
  case amb of
    
    Ok prev | prev /= "" -> makeObj result
      where          
        utterance = ok (valFromObj "utterance" jsinput) :: Utterance
        world     = ok (valFromObj "world"     jsinput) :: World
        holding   = case valFromObj "holding" jsinput of
          Ok id   -> id
          Error _ -> ""
        hold      = ok (valFromObj "hold"      jsinput) :: Id

        goals     = case getChoice utterance of
          Nothing -> []
          Just x  -> case fmap read amb of
            Error _ -> []
            Ok c    -> filterChoice x c

        plan      =
          case goals of
            []        -> error "Fucking interpreter couldn't find a goal!"
            ([]:g:gs) -> solve world hold holding g :: Plan
            (g:gs)    -> solve world hold holding g :: Plan
             
        output  = case getChoice utterance of
          Nothing -> ["I told you to give me a choice of 1-5?! Restarting!"]
          Just x  -> ["Nice choice of " ++ [intToDigit x]]
    
        clarified  = case getChoice utterance of
          Nothing -> False
          Just x  -> True
      
        result =
            [("plan",      if clarified then showJSON plan  else JSNull),
             ("output",    showJSON output),
             ("state", showJSON "")]

    _  -> makeObj result
      where       
        (wasAmbig, ambList, theChoices) =
          case resolveAmbig goals of
            Right _ -> (False, JSNull,[""])
            Left x -> (True, showJSON $ show goals, buildChoices x)
          
        utterance = ok (valFromObj "utterance" jsinput) :: Utterance
        
        world = ok (valFromObj "world" jsinput) :: World

        holding = case valFromObj "holding" jsinput of
          Ok id -> id
          Error _ -> ""

        hold      = ok (valFromObj "hold"      jsinput) :: Id

        objects   = ok (valFromObj "objects"   jsinput) :: Objects

        trees = parse command utterance :: [Command]

        goals = filter (/= []) [goal | tree <- trees, goal <- interpret world holding objects tree] :: [[PDDL]]

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
        
        result = [("utterance", showJSON utterance),
                   ("trees", showJSON (map show trees)),
                   ("goals", if length trees >= 1 then showJSON (map show goals) else JSNull),
                   ("plan", if length goals == 1 then showJSON plan else JSNull),
                   ("output", showJSON output)
                  ] ++ if wasAmbig then [("state", ambList)] else []

getChoice :: [String] -> Maybe Int
getChoice [] = Nothing
getChoice [[x]]
  | isDigit x
    && digitToInt x >= 1
    && digitToInt x < 6 = Just (digitToInt x)
  | otherwise = Nothing

filterChoice :: Int -> PDDLWorld -> PDDLWorld
filterChoice x yss =
  case resolveAmbig yss of
    Right b -> [b]
    Left  (Ambiguity atype id list) ->
      fltrGl (atype, id, list !! (x-1)) yss

fltrGl :: (AmbType, Id, Id) -> PDDLWorld -> PDDLWorld
fltrGl (t, i1, i2) = filter flt
  where
    flt = case t of
      Source -> elemBy (\(PDDL _ id1 id2) -> i1 == id2 && i2 == id1)
      _      -> elemBy (\(PDDL _ id1 id2) -> i1 == id1 && i2 == id2)

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy f (x:xs) | f x       = True
                | otherwise = elemBy f xs
elemBy _ _                  = False
