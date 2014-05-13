module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe

data AmbType = Source | Dest
data Ambiguity = Ambiguity AmbType Id [Id]

test :: PDDLWorld
test = convertWorld complexWorld

---------------------------------------------------------------------------

resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig []     = error "Not Possible"
resolveAmbig [x]    = Right x
resolveAmbig (x:xs) =
  case checkSrcDups x xs of
    Nothing -> case checkDstDups x xs of
      Nothing -> error "There must be ambiguity!"
      Just d  -> Left d
    Just s  -> Left s
    
checkDstDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkDstDups [] [] = Nothing
checkDstDups x  [] = Nothing
checkDstDups [] (y:ys) = checkDstDups y ys
checkDstDups x xs  =
  case dstList of
    Nothing  -> checkDstDups (tail x) xs
    Just amb -> Just amb
    where
        dstList = findAllDstDups (head x) xs
        
findAllDstDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllDstDups x []     = Nothing
findAllDstDups x xs =
  let ys = concatMap (dstDups x) xs
      in if null ys
      then Nothing
      else (Just (Ambiguity Dest src ys))
           where
             (PDDL _ src _) = x

dstDups :: PDDL -> [PDDL] -> [Id]
dstDups x [] = []
dstDups (PDDL rel src dst) xs =
  dst : [dst' | (PDDL rel' src' dst') <- xs, src == src']

checkSrcDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkSrcDups [] [] = Nothing
checkSrcDups x  [] = Nothing
checkSrcDups [] (y:ys) = checkSrcDups y ys
checkSrcDups x xs  =
  case srcList of
    Nothing  -> checkSrcDups (tail x) xs
    Just amb -> Just amb
    where
        srcList = findAllSrcDups (head x) xs
    
findAllSrcDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllSrcDups x []     = Nothing
findAllSrcDups x xs =
  let ys = concatMap (srcDups x) xs
      in if null ys
      then Nothing
      else (Just (Ambiguity Source dst ys))
           where (PDDL _ _ dst) = x

srcDups :: PDDL -> [PDDL] -> [Id]
srcDups x [] = []
srcDups (PDDL rel src dst) xs =
  src : [src' | (PDDL rel' src' dst') <- xs, dst == dst']
  
buildChoices :: Ambiguity -> String
buildChoices (Ambiguity isDst id listId) = case isDst of
  Dest   -> "Ambiguity error! Specify by entering a number: "
            ++ printObjects listId 1
  Source -> "Ambiguity error! Specify by entering a number: "
            ++ printObjects listId 1
  
printObjects :: [Id] -> Int -> String
printObjects [] _ = []
printObjects (x:xs) n = (show n ++ ". "
                         ++ drop 7 (show (getObjId x))
                         ++ printObjects xs (n+1))
