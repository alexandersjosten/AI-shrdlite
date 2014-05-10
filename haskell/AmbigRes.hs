module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe

data Ambiguity = Ambiguity [Id] Id

-- For testing only (will be removed later!)
testAmbigA :: [PDDL]
testAmbigA = [PDDL Ontop "a" "b", PDDL Ontop "a" "c", PDDL Ontop "b" "c"]

testAmbigB :: [PDDL]
testAmbigB = [PDDL Ontop "a" "c", PDDL Ontop "b" "c", PDDL Ontop "c" "d"]

testAmbigNone :: [PDDL]
testAmbigNone = [PDDL Ontop "a" "b", PDDL Ontop "b" "c", PDDL Ontop "c" "d"]

---------------------------------------------------------------------------

-- Check for any of the 2 ambiguous situation
resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig [] = error "impossibru"
resolveAmbig [x] =  Right x
resolveAmbig xs = undefined

buildChoices :: Ambiguity -> String
buildChoices _ = "Hello!"

-- Check for any of the 2 ambiguous situation
isAmbiguous :: [PDDL] -> Either Ambiguity [PDDL]
isAmbiguous [] = error "IMPOSSIBRU"
isAmbiguous xs =
  case multiSrc of
    Nothing -> case multiDst of
        Nothing      -> Right xs
        Just amb     -> Left amb -- TODO
    Just amb -> Left amb -- TODO
  where (PDDL _ id1 id2) = head xs
        multiSrc         = checkSrcDups (id1, id2) (tail xs)
        multiDst         = checkDstDups (id1, id2) (tail xs)

checkSrcDups :: (Id, Id) -> [PDDL] -> Maybe Ambiguity
checkSrcDups _ [] = Nothing
checkSrcDups (id1, id2) xs =
  let ys =  [id2' | (PDDL _ id1' id2') <- xs, id1 == id1']
  in if null ys then
       let (PDDL _ id' id'') = head xs
       in checkSrcDups (id',id'') (tail xs)
     else
       Just $ Ambiguity (id2:ys) id1
    

checkDstDups :: (Id, Id) -> [PDDL] -> Maybe Ambiguity
checkDstDups _          [] = Nothing
checkDstDups (id1, id2) xs =
  let ys =  [id1' | (PDDL _ id1' id2') <- xs, id2 == id2']
  in if null ys then
       let (PDDL _ id' id'') = head xs
       in checkDstDups (id', id'') (tail xs)
     else
       Just $ Ambiguity (id1:ys) id2
