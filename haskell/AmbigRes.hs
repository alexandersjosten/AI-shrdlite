module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe

-- For testing only (will be removed later!)
testAmbigA :: [PDDL]
testAmbigA = [PDDL Ontop "a" "b", PDDL Ontop "a" "c", PDDL Ontop "b" "c"]

testAmbigB :: [PDDL]
testAmbigB = [PDDL Ontop "a" "c", PDDL Ontop "b" "c", PDDL Ontop "c" "d"]

testAmbigNone :: [PDDL]
testAmbigNone = [PDDL Ontop "a" "b", PDDL Ontop "b" "c", PDDL Ontop "c" "d"]

---------------------------------------------------------------------------

-- Check for any of the 2 ambiguous situation
isAmbiguous :: [PDDL] -> Maybe String
isAmbiguous [] = Nothing
isAmbiguous xs =
  case a of
    Nothing      ->
      case b of
        Nothing      -> Nothing
        Just (is, i) -> Nothing -- TODO
    Just (is, i) -> Nothing -- TODO
  where (PDDL _ id1 id2) = head xs
        a                = checkForDups (id1, id2) (tail xs)
        b                = checkForDups' (id1, id2) (tail xs)

checkForDups :: (Id, Id) -> [PDDL] -> Maybe ([Id],Id)
checkForDups _ [] = Nothing
checkForDups (id1, id2) xs =
  let ys =  [id2' | (PDDL _ id1' id2') <- xs, id1 == id1']
  in if null ys then
       let (PDDL _ id' id'') = head xs
       in checkForDups (id',id'') (tail xs)
     else
       Just (id2:ys,id1)
    

checkForDups' :: (Id, Id) -> [PDDL] -> Maybe ([Id],Id)
checkForDups' _          [] = Nothing
checkForDups' (id1, id2) xs =
  let ys =  [id1' | (PDDL _ id1' id2') <- xs, id2 == id2']
  in if null ys then
       let (PDDL _ id' id'') = head xs
       in checkForDups (id', id'') (tail xs)
     else
       Just (id1:ys,id2)
