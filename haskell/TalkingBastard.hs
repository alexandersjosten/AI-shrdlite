module TalkingBastard where 


import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List
import Data.Char
import Data.Maybe


import HelpFunctions



amIAlone :: Id -> PDDLWorld -> String
amIAlone i w =  if or $ checkForm (Object s c f) objects
                then 
                        if or $ checkColor (Object s c f) objects  
                        then 
                            if or $ checkSize (Object s c f) objects 
                            then show s ++ " " ++ show c  ++ " " ++ show f
                            else show s  ++ " " ++ show f  
                        else show c ++ " "  ++ show f  
                            
                else show f
            where
                checkForm _ []                                     = []
                checkForm (Object s1 c1 f1) ((Object s2 c2 f2):os) = (f1 == f2) : checkForm (Object s1 c1 f1) os
                checkColor _ []                                     = []
                checkColor (Object s1 c1 f1) ((Object s2 c2 f2):os) = (f1 == f2 && c1 == c2) : checkColor (Object s1 c1 f1) os
                checkSize _ []                                     = []
                checkSize (Object s1 c1 f1) ((Object s2 c2 f2):os) = (f1 == f2 && s1 == s2) : checkSize (Object s1 c1 f1) os
                (Object s c f)  = getObjId i
                objects = (convertToObjects w)
                
                
startSentence :: Int -> String
startSentence x | x < 3     =   "I can do this! It will only take " ++  show x ++ " moves. I start by moving the "
                | x < 6     =   "This is almost a challenge, even Peter could do this! just " ++  show x ++ " moves. I start by moving the "
                | otherwise =   "This is a hard one! It will take me all day to move " ++  show x ++ " objects! Ohhh well lets get started... I start by moving the "

moveSentence :: Id -> String
moveSentence id = case getObjId id of
            (Object s c Box) -> "Ohhh this is heavy, I move "
            (Object s c Ball) -> "Slippery shiiit, I move "
            (Object s c Brick) -> "I don't want to move this one, ohh hell, I move " 
            (Object s c Pyramid) ->"A pyramid, wtf, I move "
            (Object s c Plank) ->"Fuff, it's hard to balance this one, I move  "
            (Object s c Table) ->"Akwardddd,,, I move "

startTB :: [Move] -> PDDLWorld -> Plan
startTB [] _         = ["Not possible!"]
startTB [(99,99)] _  = ["It is free! EASY!"]
startTB ((x,y):[]) w 
					| x> 13    = ["No way! I'm not doing this! I'm expecting a minimum of " ++ show x ++ " moves! This must be impossible"]
					|otherwise = do
                 let (i,nw) = take' x w
                 let stack =  (w !! y)
                 let finaltext = case stack of
                        [] -> " on the floor"
                        ((PDDL Ontop a b):c) ->  " on the " ++  (amIAlone a (snd (take' y nw)))
                 ["This is a easy one! I just move the " ++ amIAlone i nw ++  finaltext , " pick " ++  show x , "drop " ++ show y]
startTB ((x,y):ms) w = do
                 let (i,nw) = take' x w
                 let nw'    = drop' y nw i
                 [startSentence (length ((x,y):ms))  ++ amIAlone i nw ++ " then  " , " pick " ++  show x , "drop " ++ show y  ] ++ talkingBastard ms nw'


talkingBastard :: [Move] -> PDDLWorld -> Plan
talkingBastard ((x,y):[]) w = do
                 let (i,nw) = take' x w
                 let stack =  (w !! y)
                 let finaltext = case stack of
                        [] -> " on the floor"
                        ((PDDL Ontop a b):c) ->  " on the " ++  (amIAlone a (snd (take' y nw)))
                 ["finally I move the " ++ amIAlone i nw ++  finaltext , " pick " ++  show x , "drop " ++ show y]
talkingBastard ((x,y):ms) w = do
                 let (i,nw) = take' x w
                 let nw'    = drop' y nw i
                 [moveSentence i ++ amIAlone i nw , " pick " ++  show x , "drop " ++ show y] ++ talkingBastard ms nw'
