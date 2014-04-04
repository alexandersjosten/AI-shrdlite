module Test where

import Text.JSON
import ShrdliteGrammar hiding (Object, size, color, form)
import HelpFunctions hiding (listOfObjects, Object)

data Object = Object { size  :: Size
                     , color :: Color
                     , form  :: Form
                     }
            deriving Show

type Map = [(Id, Object)]

testObject :: Object
testObject = Object {size = AnySize, color = AnyColor, form = Plank}

listOfObjects :: Map
listOfObjects = [("a", Object { size = Large, color = Green,  form = Brick})
                ,("b", Object { size = Small, color = White,  form = Brick}) 
                ,("c", Object { size = Large, color = Red,    form = Plank})  
                ,("d", Object { size = Small, color = Green,  form = Plank }) 
                ,("e", Object { size = Large, color = White,  form = Ball }) 
                ,("f", Object { size = Small, color = Black,  form = Ball })   
                ,("g", Object { size = Large, color = Blue,   form = Table })  
                ,("h", Object { size = Small, color = Red,    form = Table })
                ,("i", Object { size = Large, color = Yellow, form = Pyramid })
                ,("j", Object { size = Small, color = Red,    form = Pyramid })
                ,("k", Object { size = Large, color = Yellow, form = Box })
                ,("l", Object { size = Large, color = Red,    form = Box })
                ,("m", Object { size = Small, color = Blue,   form = Box }) 
                ]

{-
getAll :: Object -> Map -> [Object]
getAll o =
  if size o == AnySize && color o == AnyColor then
     filter (\o' -> form o' == form') . map snd
   else
    if size o == AnySize then
       filter (\o' -> form o' == form' && color o' == color') . map snd
    else
      if color o == AnyColor then
        filter (\o' -> form o' == form' && size o' == size') . map snd
      else
        filter (\o' -> form o' == form' && size o' == size' && color o' == color') . map snd
  where form'  = form o
        color' = color o
        size'  = size o
-}

getAll :: Object -> Map -> [Object]
getAll o =
  case o of
    (Object AnySize AnyColor _) -> filter (\o' -> form o' == form') . map snd
    (Object AnySize _        _) -> filter (\o' -> form o' == form' && color o' == color') . map snd
    (Object _       AnyColor _) -> filter (\o' -> form o' == form' && size o' == size') . map snd
    _                           -> filter (\o' -> form o' == form' && size o' == size' && color o' == color') . map snd
  where form'  = form o
        color' = color o
        size'  = size o
