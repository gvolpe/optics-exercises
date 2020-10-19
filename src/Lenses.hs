{-# LANGUAGE RankNTypes, TemplateHaskell #-}

module Lenses where

import Control.Lens

-- Lenses 3.1 - Optics Anatomy
-- action: view
-- path: (_1 . _2)
-- structure: ((1, 2), 3)
-- focus: 2
lenses_3_1 = view (_1 . _2) ((1, 2), 3) -- result = 2

lenses_3_2_1 :: Lens' (Bool, (Int, String)) Int
lenses_3_2_1 = undefined

lenses_3_2_2 :: Lens' (Char, Int) Char
lenses_3_2_2 = undefined

lenses_3_2_3 = [ "view", "over", "set" ]

lenses_3_2_4 = view _3 ('a', 'b', 'c')

lenses_3_2_5 = over _2 (*10) (False, 2) -- result = (False, 20)

lenses_3_2_5_tuple_2 :: Lens' (Bool, Int) Int
lenses_3_2_5_tuple_2 = _2

lenses_3_2_5_over :: Lens' (Bool, Int) Int -> (Int -> Int) -> (Bool, Int) -> (Bool, Int)
lenses_3_2_5_over = over

data Ship = Ship
  { _name :: String
  , _numCrew :: Int
  } deriving Show

makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew =
  ship { _numCrew = newNumCrew }

__numCrew :: Lens' Ship Int
__numCrew = lens getNumCrew setNumCrew

__name :: Lens' Ship String
__name = lens _name (\s a -> s { _name = a })

myShip = Ship "PurpleShip" 30

lenses_3_3_1 = view numCrew myShip

lenses_3_3_2_1 = [ "wand", "book", "potions" ] -- generated lenses
lenses_3_3_2_2 = "gazor :: Lens' Chumble Spuzz"
lenses_3_3_2_3 = "move the makeLenses call right below the record definition"
