module AmbigRes where

import ShrdliteGrammar
import HelpFunctions


--Ambiguity for when an object is to be placed on top of several objects at once
isAmbiguousOn :: [PDDL] -> Bool
isAmbiguousOn [] = False
isAmbiguousOn ((PDDL _ a _):xs) =
  if null [y | y@(PDDL _ a' _) <- xs, a == a'] then
    --We found no ambiguity for a, so we want to check the rest of the list
    isAmbiguous xs
  else
    --We found an ambiguity so we want to point out which object it concerns.
    --Ask the user where it wants to put the ambious object. TODO
    True

--Ambiguity for when an object is to be supporting many objects at once
isAmbiguousUnder :: [PDDL] -> Bool
isAmbiguousUnder [] = False
isAmbiguousUnder ((PDDL _ _ a):xs) =
  if null [y | y@(PDDL _ _ a') <- xs, a == a'] then
    --We found no ambiguity for a, so we want to check the rest of the list
    isAmbiguous xs
  else
    --We found an ambiguity so we want to point out which object it concerns.
    --Ask the user where it wants to put the ambious object. TODO
    True


