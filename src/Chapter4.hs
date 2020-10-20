-- Polymorphic Optics
module Chapter4 where

import           Control.Lens

data Vorpal a = Vorpal a

poly_4_2_1 :: Lens (Vorpal a) (Vorpal b) a b
poly_4_2_1 = undefined

data Preferences a b = Preferences
  { _best :: a
  , _worst :: b
  } deriving Show

poly_4_2_2 :: Lens (Preferences a b) (Preferences a' b) a a'
poly_4_2_2 = lens getter setter
 where
  getter = _best
  setter p x = let y = _best p in p { _best = x }

data Result e = Result
  { _lineNumber :: Int
  , _result :: Either e String
  }

poly_4_2_3 :: Lens (Result a) (Result b) (Either a String) (Either b String)
poly_4_2_3 = undefined

poly_4_2_4 = undefined -- hmm not sure how...

data Predicate a = Predicate (a -> Bool)

poly_4_2_5 :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
poly_4_2_5 = lens getter setter
 where
  getter (Predicate f) = f
  setter _ g = Predicate g
