module Interpreter where

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)


import HelpFunctions


interpret :: World -> Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = [True]
