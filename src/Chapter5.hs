{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- Operators
module Chapter5 where

import           Control.Lens
import           Data.Char                      ( toUpper )

data Gate = Gate
  { _open :: Bool
  , _oilTemp :: Float
  } deriving Show
makeLenses ''Gate

data Army = Army
  { _archers :: Int
  , _knights :: Int
  } deriving Show
makeLenses ''Army

data Kingdom = Kingdom
  { _name :: String
  , _army :: Army
  , _gate :: Gate
  } deriving Show
makeLenses ''Kingdom

-- Exercise: 5.9.1
duloc :: Kingdom
duloc = Kingdom "Duloc" x y
 where
  x = Army 22 14
  y = Gate True 10.0

goalA :: Kingdom
goalA = duloc
  & name <>~ ": a perfect place"
  & army . knights *~ 3
  & gate . open &&~ False

goalB :: Kingdom
goalB = duloc
  & name <>~ "instein"
  & army . archers .~ 17
  & army . knights .~ 26
  & gate . oilTemp ^~ 2

goalC :: (String, Kingdom)
goalC = name <<<>~ " of the talking Donkeys" $ duloc
  & name <>~ ": Home"
  & gate . oilTemp //~ 2

ops_5_9_2_1 = (False, "opossums") & _1 ||~ True

ops_5_9_2_2 = 2 & id *~ 3

ops_5_9_2_3 = ((True, "Dudley"), 55.0)
  & _1 . _2 <>~ " - the worst"
  & _2 -~ 15
  & _2 //~ 2
  & _1 . _2 %~ map toUpper
  & _1 . _1 .~ False

ops_5_9_2_4 :: s -> Lens' s a -> a
ops_5_9_2_4 = (^.)

ops_5_9_2_5 :: Lens' s a -> (a -> a) -> s -> s
ops_5_9_2_5 = (%~)
