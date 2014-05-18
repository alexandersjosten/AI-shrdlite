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

import System.IO.Unsafe

main :: IO ()
main = do
  input <- getContents
  putStrLn . encode . jsonMain . ok $  decode input

jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput =
  let amb = (valFromObj "state" jsinput) :: Result String in
  case amb of
    Ok prev | prev /= "" -> makeObj result
      where
        (wasAmbig, ambList, theChoices) =
          case resolveAmbig goals of
            Right _ -> (False, JSNull,"")
            Left x  -> (True, showJSON $ show goals, buildChoices x)

        utterance = ok (valFromObj "utterance" jsinput) :: Utterance
        world     = ok (valFromObj "world"     jsinput) :: World
        holding   = case valFromObj "holding" jsinput of
          Ok id   -> id
          Error _ -> ""

        hold      = ok (valFromObj "hold"      jsinput) :: Id

        prevGoals = fmap read amb

        goals
          | clarified = case prevGoals of
            Error _ -> error "!"
            Ok c    -> case resolveAmbig c of
              Right a -> [a]
              Left b  -> case getChoice utterance b of
                Nothing -> error "!!"
                Just x  -> filterChoice x c
          | otherwise  = case prevGoals of
            Error _ -> error "!"
            Ok a -> a

        plan      =
          case goals of
            []        -> error "Fucking interpreter couldn't find a goal!"
            ([]:g:gs) -> solve world hold holding g :: Plan
            (g:gs)    -> solve world hold holding g :: Plan
             
        clarified = case prevGoals of
            Error _ -> False
            Ok c -> case resolveAmbig c of
              Left a -> case getChoice utterance a of
                Nothing -> False
                Just x  -> True
              Right b -> True
    
        output
          | not clarified = "Ehh, why not trying to chose something that exist? Restarting."
          | clarified && wasAmbig = case resolveAmbig goals of
              Right a -> error "EH WHAT?"
              Left b -> "There is some ambiguity left, " ++ drop 16 (theChoices)
          | otherwise = "Great!"
      
        result =
            [("utterance", showJSON utterance),
             ("plan", if clarified && not wasAmbig then showJSON plan else JSNull),
             ("output", showJSON output)]
            ++ if wasAmbig && clarified then [("state", ambList)]
               else [("state", showJSON "")]

    _  -> makeObj result
      where       
        (wasAmbig, ambList, theChoices) =
          case resolveAmbig goals of
            Right _ -> (False, JSNull,"")
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
          | null trees = "Parse error!"
          | null goals = "Interpretation error!"
          | wasAmbig = case (length goals > 5) of
                            True -> "There are "
                                     ++ show (length goals)
                                     ++ " things you could mean, please be more specific!"
                            _    -> theChoices
          | null plan = "Planning error!"
          | otherwise = "Success!"
        
        result = [("utterance", showJSON utterance),
                   ("trees", showJSON (map show trees)),
                   ("goals", if length trees >= 1 then showJSON (map show goals) else JSNull),
                   ("plan", if length goals == 1 then showJSON plan else JSNull),
                   ("output", showJSON output)
                  ] ++ if wasAmbig && ((length goals) <= 5) then [("state", ambList)] else [("state", showJSON "")]

getChoice :: [String] -> Ambiguity -> Maybe Int
getChoice [] _ = Nothing
getChoice [[x]] (Ambiguity _ _ idlist)
  | isDigit x
    && digitToInt x >= 1
    && digitToInt x < 6
    && digitToInt x <= length idlist = Just (digitToInt x)
  | otherwise = Nothing
getChoice _ _ = Nothing

filterChoice :: Int -> PDDLWorld -> PDDLWorld
filterChoice x yss = 
  case resolveAmbig yss of
    Right b -> [b]
    Left  (Ambiguity atype id list) ->
      fltrGl (atype, list !! (x-1)) yss

fltrGl (t, i2) = filter flt
  where
    flt = case t of
      Source -> elemBy (\(PDDL _ id1 id2) -> i2 == id1)
      _      -> elemBy (\(PDDL _ id1 id2) -> i2 == id2)

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy f (x:xs) | f x       = True
                | otherwise = elemBy f xs
elemBy _ _                  = False
