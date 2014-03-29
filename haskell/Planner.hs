module Planner where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)


import HelpFunctions



solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = ["I picked it up . . .", "pick " ++ show col, ". . . and I dropped it down", "drop " ++ show col]
    where
      Just col = findIndex (not . null) world
