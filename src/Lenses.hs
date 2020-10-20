{-# LANGUAGE RankNTypes, TemplateHaskell #-}

module Lenses where

import           Control.Lens
import           Control.Lens.Unsound           ( lensProduct )

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

lenses_3_2_3 = ["view", "over", "set"]

lenses_3_2_4 = view _3 ('a', 'b', 'c')

lenses_3_2_5 = over _2 (* 10) (False, 2) -- result = (False, 20)

lenses_3_2_5_tuple_2 :: Lens' (Bool, Int) Int
lenses_3_2_5_tuple_2 = _2

lenses_3_2_5_over
  :: Lens' (Bool, Int) Int -> (Int -> Int) -> (Bool, Int) -> (Bool, Int)
lenses_3_2_5_over = over

data Ship = Ship
  { _name :: String
  , _numCrew :: Int
  } deriving Show

makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship { _numCrew = newNumCrew }

__numCrew :: Lens' Ship Int
__numCrew = lens getNumCrew setNumCrew

__name :: Lens' Ship String
__name = lens _name (\s a -> s { _name = a })

myShip = Ship "PurpleShip" 30

lenses_3_3_1 = view numCrew myShip

lenses_3_3_2_1 = ["wand", "book", "potions"] -- generated lenses
lenses_3_3_2_2 = "gazor :: Lens' Chumble Spuzz"
lenses_3_3_2_3 = "move the makeLenses call right below the record definition"

lenses_3_4_1 :: Lens' (a, b, c) b
lenses_3_4_1 = _2

lenses_3_4_2 :: Lens' (Maybe a) a
lenses_3_4_2 = undefined -- Not possible, it's a Prism: _Just

lenses_3_4_3 :: Lens' (Either a b) a
lenses_3_4_3 = undefined -- Not possible, also a Prism: _Left. If we have `Either a a` instead, we could use `chosen`.

-- focus on third element of a list
lenses_3_4_4 :: Lens' [a] a
lenses_3_4_4 = undefined -- Not possible, it's a Traversal.

-- conditional: if (_1) then _2 else _3
lenses_3_4_5 :: Lens' (Bool, a, a) a
lenses_3_4_5 = lens getter setter
 where
  getter = \(b, a1, a2) -> if b then a1 else a2
  setter = \(b, a1, a2) a' -> if b then (b, a', a2) else (b, a1, a')

data Err = ReallyBadError { _msg :: String } | ExitCode { _code :: Int }
makeLenses ''Err

lenses_3_4_6_msg :: Lens' Err String
lenses_3_4_6_msg = undefined -- Not possible. Needs a Prism to focus on `ReallyBadError` and then apply the `msg` lens.

type UserId = String
type UserName = String

data Session = Session
  { _userId :: UserId
  , _userName :: UserName
  , _createdAt :: String
  , _expiresAt :: String
  } deriving (Eq, Show)
makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- skipping laws exercises... boring

-- virtual fields --
data User = User
  { _firstName :: String
  , _lastName :: String
  --, _username :: String
  , _email :: String
  } deriving Show
makeLenses ''User

-- lenses_3_6_1
username :: Lens' User String
username = email

-- lenses_3_6_2
fullName :: Lens' User String
fullName = lens getter setter
 where
  fl = \s -> case words s of
    (x : xs) -> (x, unwords xs)
    _        -> (s, "")
  getter u = view firstName u <> " " <> view lastName u
  setter u x =
    let fl' = fl x
    in  set lastName (view _2 fl') $ set firstName (view _1 fl') u

someUser = User "John" "Cena" "invisible@example.com"

getFullName = view fullName someUser
setFullName = set fullName "Doctor of Thuganomics" someUser

-- validation lenses --
data ProducePrices = ProducePrices
  { _limePrice :: Float
  , _lemonPrice :: Float
  } deriving Show

-- lenses_3_7_1 (same for lemonPrice)
limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
 where
  getter = _limePrice
  setter p x = p { _limePrice = max 0 x }

-- lenses_3_7_2
limePrice' :: Lens' ProducePrices Float
limePrice' = lens getter setter
 where
  getter p = _limePrice p
  setter p x =
    let p'  = set limePrice x p
        lim = view limePrice p'
        lem = _lemonPrice p'
        dif = abs (lim - lem)
        f   = 0.5
    in  if dif <= f
          then p'
          else p' { _lemonPrice = if lim < lem then lim + f else lim - f }
