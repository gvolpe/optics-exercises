{-# LANGUAGE OverloadedStrings #-}

-- Indexed Optics
module Chapter11 where

import           Control.Lens
import qualified Data.Map                      as M

indexed1 = itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"] -- [(0,"Summer"),(1,"Fall"),(2,"Winter"),(3,"Spring")]

agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]

indexed2 = agenda ^@.. itraversed -- [("Monday","Shopping"),("Tuesday","Swimming")]

indexed3 = (True, "value") ^@.. itraversed -- [(True,"value")]

agenda' = M.fromList [("Monday" , ["Shopping", "Yoga"]) , ("Saturday", ["Brunch", "Food coma"])]

indexed4 = agenda' ^@.. itraversed . itraversed -- [(0,"Shopping"), (1,"Yoga"), (0,"Brunch"), (1,"Food coma")]

indexed5 = agenda' ^@.. itraversed <. itraversed -- [("Monday","Shopping"),("Monday","Yoga"),("Saturday","Brunch"),("Saturday","Food coma")]

-- keeping both indices
indexed6 = agenda' ^@.. itraversed <.> itraversed -- [(("Monday",0),"Shopping"),(("Monday",1),"Yoga"),(("Saturday",0),"Brunch"),(("Saturday",1),"Food coma")]

showDayAndNumber :: String -> Int -> String
showDayAndNumber a b = a <> ": " <> show b

indexed7 = agenda' ^@.. icompose showDayAndNumber itraversed itraversed -- [("Monday: 0","Shopping"),("Monday: 1","Yoga"),("Saturday: 0","Brunch"),("Saturday: 1","Food coma")]

--itoListOf    (%@∼)
--iover        (%%@∼)
--itraverseOf  (^@..)
--iset         (.@∼)
--iview        (^@.)

exercises_1_1 = M.fromList [("streamResponse", False), ("useSSL", True)] ^@.. itraversed -- [("streamResponse",False),("useSSL",True)]

exercises_1_2 = (M.fromList [('a', 1), ('b', 2)], M.fromList [('c', 3), ('d', 4)]) ^@.. both . itraversed -- [('a',1),('b',2),('c',3),('d',4)]

exercises_1_3 = M.fromList [('a', (True, 1)), ('b', (False, 2))] ^@.. itraversed <. _1 -- [('a',True),('b',False)]

exercises_1_4 = [M.fromList [("Tulips", 5), ("Roses", 3)], M.fromList [("Goldfish", 11), ("Frogs", 8)]] ^@.. itraversed <.> itraversed -- [((0,"Roses"),3),((0,"Tulips"),5),((1,"Frogs"),8),((1,"Goldfish"),11)]

exercises_1_5 = [10, 20, 30] & itraversed %@~ (+) -- [10,21,32]

exercises_1_6 = itraverseOf_ itraversed (\i s -> putStrLn (replicate i ' ' <> s)) ["one", "two", "three"] -- one two three

exercises_1_7 = itraverseOf_ itraversed (\n s -> putStrLn $ show n <> ": " <> s) ["Go shopping", "Eat lunch", "Take a nap"] -- 0: Go shopping\n 1: Eath lunch\n 2: Take a nap
