#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)

import HelpFunctions -- and types
import Interpreter
import Planner


main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode

--Test Goals

--Put the white ball(e) in the large yellow box(k) beside the large red box(l) beside the large yellow pyramid(i)
testGoals1 = [PDDL Ontop "e" "k", PDDL Beside "e" "l",  PDDL Beside "l" "i"]
-- Put the black ball in the small blue box ontop of the large yellow box beside the large green brick
testGoals2 = [PDDL Ontop "f" "m", PDDL Beside "m" "a", PDDL Ontop "m" "k"]

jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput) :: Utterance
      world     = ok (valFromObj "world"     jsinput) :: World
      holding   = ok (valFromObj "holding"   jsinput) :: Id
      hold      = ok (valFromObj "hold"      jsinput) :: Id
      objects   = ok (valFromObj "objects"   jsinput) :: Objects

      trees     = parse command utterance :: [Command]

      goals     = [goal | tree <- trees, goal <- interpret world holding objects tree] :: [Goal]
      goals'     = [goal | tree <- trees, goal <- interpret' tree] :: [PDDL]
      
      
      

      plan      = solve world hold holding goals' :: Plan

      output    = if null trees then "Parse error!"
                  else if null goals' then "Interpretation error!"
                       else if length goals >= 2 then "Ambiguity error!"
                            else if null plan then "Planning error!"
                                 else "Success!"

      result    = [("utterance", showJSON utterance),
                   ("trees",     showJSON (map show trees)),
                   ("goals",     if length trees >= 1 then showJSON (map show goals') else JSNull),
                   ("plan",      if length goals == 1 then showJSON plan  else JSNull),
                   ("output",    showJSON output)
                  ]

