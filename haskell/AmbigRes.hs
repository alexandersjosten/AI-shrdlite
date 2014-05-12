module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe

data Ambiguity = Ambiguity Bool Id [Id]

test :: PDDLWorld
test = convertWorld complexWorld

---------------------------------------------------------------------------

resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig []     = error "Not Possible"
resolveAmbig [x]    = Right x
resolveAmbig (x:xs) =
  case multiDst of
    Nothing   -> case multiSrc of
      Nothing   -> resolveAmbig xs
      Just ambD -> Left ambD
    Just ambS -> Left ambS
    where
      multiSrc = checkSrcDups x xs
      multiDst = checkDstDups x xs


checkDstDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkDstDups [] [] = Nothing
checkDstDups [] (x:xs) = checkDstDups x xs
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
      else (Just (Ambiguity True src ys))
           where
             (PDDL _ src _) = x

dstDups :: PDDL -> [PDDL] -> [Id]
dstDups x [] = []
dstDups (PDDL rel src dst) xs =
  let ys = [dst' | (PDDL _ src' dst') <- xs, src == src']
      in if null ys
      then dstDups (PDDL rel src dst) (tail xs)
      else ys ++ dstDups (PDDL rel src dst) (tail xs)

checkSrcDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkSrcDups [] [] = Nothing
checkSrcDups [] (x:xs) = checkSrcDups x xs
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
      else (Just (Ambiguity False dst ys))
           where (PDDL _ _ dst) = x

srcDups :: PDDL -> [PDDL] -> [Id]
srcDups x [] = []
srcDups (PDDL rel src dst) xs =
  let ys = [src' | (PDDL _ src' dst') <- xs, dst == dst']
      in if null ys
      then srcDups (PDDL rel src dst) (tail xs)
      else ys ++ srcDups (PDDL rel src dst) (tail xs)

buildChoices :: Ambiguity -> String
buildChoices (Ambiguity isDst id listId) = case isDst of
  True  -> "Destination Ambiguity"
  False -> "Source Ambiguity"
  

