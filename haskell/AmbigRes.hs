module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe
import Data.Char

-- The data types needed for modelling an ambiguity
data AmbType = Source | Dest deriving (Show, Eq, Read)
data Ambiguity = Ambiguity AmbType Id [Id] deriving (Show)

---------------------------------------------------------------------------
--Top-level function that calls the helper functions
--It calls the source search first and if resultless, the destination search
--Precond: The list of lists of PDDLs can not be empty
--Output: A singel goal in a list or an ambiguity
resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig []     = error "Not Possible" --Precond
resolveAmbig [x]    = Right x
resolveAmbig (x:xs) =
  case checkSrcDups x xs of
    Nothing -> case checkDstDups x xs of
      Nothing -> case allSources (x:xs) of --Testing impossible worlds.
        Nothing -> Right x
        Just a -> Left a 
      Just d  -> Left d --Destination amb.
    Just s  -> Left s --Source amb.


--For testing only in impossible cases
allSources :: [[PDDL]] -> Maybe Ambiguity
allSources [] = Nothing
allSources xs = Just (Ambiguity Source "" ys)
  where ys = [ id | y <- xs, (PDDL _ id id2) <- y]
  

--Find all destination ambiguitys between target goal
--in a list and the rest of the goals in the list        
checkDstDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkDstDups [] [] = Nothing
checkDstDups x  [] = Nothing
checkDstDups [] (y:ys) = checkDstDups y ys
checkDstDups x xs  =
  case dstList of
    Nothing  -> checkDstDups (tail x) xs --recursive step
    Just amb -> Just amb --found amb.
    where
        dstList = findAllDstDups (head x) xs --helper function call

--Function for comparing one PDDL with the rest of the PDDLs
--in destination ambiguity
findAllDstDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllDstDups x []     = Nothing
findAllDstDups x xs =
  let ys = concatMap (dstDups x) xs --append each list of id's that is generated
      in if null ys
      then Nothing
      else (Just (Ambiguity Dest src (dst:ys)))
           where
             (PDDL _ src dst) = x
             
--Compare PDDLs to see if the source matches but destination does not
--Output is the list of all destination ids that did not match  
dstDups :: PDDL -> [PDDL] -> [Id]
dstDups x [] = []
dstDups (PDDL rel src dst) xs =
  [dst' | (PDDL rel' src' dst') <- xs, src == src']

--Find all source ambiguitys between target goal
--in a list and the rest of the goals in the list   
checkSrcDups :: [PDDL] -> [[PDDL]] -> Maybe Ambiguity
checkSrcDups [] [] = Nothing
checkSrcDups x  [] = Nothing
checkSrcDups [] (y:ys) = checkSrcDups y ys
checkSrcDups x xs  =
  case srcList of
    Nothing  -> checkSrcDups (tail x) xs --recursive step
    Just amb -> Just amb --found amb.
    where
        srcList = findAllSrcDups (head x) xs --helper function call

--Function for comparing one PDDL with the rest of the PDDLs
--in source ambiguity
findAllSrcDups :: PDDL -> [[PDDL]] -> Maybe Ambiguity
findAllSrcDups x []     = Nothing
findAllSrcDups x xs =
  let ys = concatMap (srcDups x) xs
      in if null ys
      then Nothing
      else (Just (Ambiguity Source dst (src:ys)))
           where (PDDL _ src dst) = x

--Compare PDDLs to see if the destination matches but source does not
--Output is the list of all sources ids that did not match                 
srcDups :: PDDL -> [PDDL] -> [Id]
srcDups x [] = []
srcDups (PDDL rel src dst) xs =
  [src' | (PDDL rel' src' dst') <- xs, dst == dst']

--Main string choice function
buildChoices :: Ambiguity -> String
buildChoices (Ambiguity isDst id listId)
  | length listId > 5 = "There is "
                         ++ show (length listId)
                         ++ " objects you could be refering to, be more specific!"
  | otherwise         = "Ambiguity error! Specify by entering a single number: "
                        ++ printObjects listId 1

--String representation of a list of objects, int used to number the list
printObjects :: [Id] -> Int -> String
printObjects [] _ = ""
printObjects ("floor":xs) n = "(" ++ show n ++ ").The floor"
                         ++ printObjects xs (n+1)
printObjects ("":xs) n = "(" ++ show n ++ ").The hook"
                         ++ printObjects xs (n+1)
printObjects (x:xs) n = "(" ++ show n ++ ").The "
                         ++ map toLower (drop 7 (show (getObjId x)))
                         ++ "  "
                         ++ printObjects xs (n+1)
