module AmbigRes where

import ShrdliteGrammar
import HelpFunctions
import Data.Maybe
import Data.Char

data AmbType = Source | Dest deriving (Show, Eq, Read)
data Ambiguity = Ambiguity AmbType Id [Id] deriving (Show)

test :: PDDLWorld
test = convertWorld complexWorld

testWorld2 = [[PDDL Ontop "f" "k"],
  [PDDL Ontop "f" "l"],
  [PDDL Ontop "f" "m"]]

---------------------------------------------------------------------------

resolveAmbig :: [[PDDL]] -> Either Ambiguity [PDDL]
resolveAmbig []     = error "Not Possible"
resolveAmbig [x]    = Right x
resolveAmbig (x:xs) =
  case checkSrcDups x xs of
    Nothing -> case checkDstDups x xs of
      Nothing -> case allSources (x:xs) of
        Nothing -> Right x
        Just a -> Left a
      Just d  -> Left d
    Just s  -> Left s

allSources :: [[PDDL]] -> Maybe Ambiguity
allSources [] = Nothing
allSources xs = Just (Ambiguity Source "" ys)
  where ys = [ id | y <- xs, (PDDL _ id id2) <- y]
  
    
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
      else (Just (Ambiguity Dest src (dst:ys)))
           where
             (PDDL _ src dst) = x

dstDups :: PDDL -> [PDDL] -> [Id]
dstDups x [] = []
dstDups (PDDL rel src dst) xs =
  [dst' | (PDDL rel' src' dst') <- xs, src == src']

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
      else (Just (Ambiguity Source dst (src:ys)))
           where (PDDL _ src dst) = x

srcDups :: PDDL -> [PDDL] -> [Id]
srcDups x [] = []
srcDups (PDDL rel src dst) xs =
  [src' | (PDDL rel' src' dst') <- xs, dst == dst']
  
buildChoices :: Ambiguity -> String
buildChoices (Ambiguity isDst id listId)
  | length listId > 5 = "There is "
                         ++ show (length listId)
                         ++ " objects you could be refering to, be more specific!"
  | otherwise         = "Ambiguity error! Specify by entering a single number: "
                        ++ printObjects listId 1
  
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
