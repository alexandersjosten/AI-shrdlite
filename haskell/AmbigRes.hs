module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe

data Ambiguity = Ambiguity Bool Id [Id]

---------------------------------------------------------------------------

resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig []     = error "impossibru"
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
checkDstDups [] xs = checkDstDups (head xs) (tail xs)
checkDstDups x xs  =
  case dstList of
    Nothing  -> checkDstDups (tail x) xs
    Just amb -> Just amb
    where
      dstList = findAllDstDups (head x) xs
    
findAllDstDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllDstDups x []     = Nothing
findAllDstDups (PDDL rel src dst) (x:xs) =
  let ys =  [dst' | (PDDL _ src' dst') <- x, src == src']
      in if null ys
      then findAllDstDups (PDDL rel src dst) xs
      else (Just (Ambiguity False src ys))

checkSrcDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkSrcDups [] [] = Nothing
checkSrcDups [] xs = checkSrcDups (head xs) (tail xs)
checkSrcDups x xs  =
  case srcList of
    Nothing  -> checkSrcDups (tail x) xs
    Just amb -> Just amb
    where
        srcList = findAllSrcDups (head x) xs
    
findAllSrcDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllSrcDups x []     = Nothing
findAllSrcDups (PDDL rel src dst) (x:xs) =
  let ys =  [src' | (PDDL _ src' dst') <- x, dst == dst']
      in if null ys
      then findAllSrcDups (PDDL rel src dst) xs
      else Just (Ambiguity True dst ys)

buildChoices :: Ambiguity -> String
buildChoices _ = "TODO!"
