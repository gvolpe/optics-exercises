{-# LANGUAGE InstanceSigs, RankNTypes, TemplateHaskell, TypeFamilies #-}

-- Indexable Structures
module Chapter8 where

import           Control.Lens
import           Data.Char                     ( toLower )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

is_8_1_1 = ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly" -- ["Larry","Wiggly","Moe"]

heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]

is_8_1_2 = heroesAndVillains & at "Spiderman" .~ Just "Goblin" -- M.fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]

is_8_1_3 = sans "Superman" heroesAndVillains -- M.fromList [("Batman","Joker")]

is_8_1_4 = S.fromList ['a', 'e', 'i', 'o', 'u']
  & at 'y' ?~ ()
  & at 'i' .~ Nothing -- S.fromList "aeouy"

-- use ix and at to go from input to output (the book fails to mention you can also use `sans`)
input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
output = M.fromList [("candy bars",13),("ice cream",5),("soda",37)]

is_8_1_5 = input & ix "soda" +~ 3 & at "ice cream" ?~ 5 & sans "gum"

------------------------------- custom indexed data structures -----------------------------------

newtype Cycled a = Cycled [a] deriving Show

type instance Index (Cycled a)   = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  --ix :: Applicative f => Int -> (a -> f a) -> Cycled a -> f (Cycled a)
  ix :: Int -> Traversal' (Cycled a) a
  ix i handler (Cycled xs) =
    Cycled <$> traverseOf (ix (i `mod` length xs)) handler xs

data Address = Address
  { _buildingNumber  :: Maybe String
  , _streetName      :: Maybe String
  , _apartmentNumber :: Maybe String
  , _postalCode      :: Maybe String
  } deriving Show
makeLenses ''Address

data AddressPiece = BuildingNumber | StreetName | ApartmentNumber | PostalCode deriving Show

type instance Index Address   = AddressPiece
type instance IxValue Address = String

instance Ixed Address
instance At Address where
  at :: AddressPiece -> Lens' Address (Maybe String)
  at BuildingNumber  = buildingNumber
  at StreetName      = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode      = postalCode

addr = Address Nothing Nothing Nothing Nothing

sherlockAddr = addr
  & at StreetName      ?~ "Baker St."
  & at ApartmentNumber ?~ "221B"

-- exercise --

newtype CiMap a = CiMap (M.Map String a) deriving Show

type instance Index (CiMap a)   = String
type instance IxValue (CiMap a) = a

instance Ixed (CiMap a) where
  ix :: String -> Traversal' (CiMap a) a
  ix i f (CiMap m) =
    CiMap <$> traverseOf (ix $ toLower <$> i) f m

instance At (CiMap a) where
  at :: String -> Lens' (CiMap a) (Maybe a)
  at i =
    let i'  = toLower <$> i
        get = \(CiMap m)   -> M.lookup i' m
        set = \(CiMap m) v -> CiMap (m & at i' .~ v)
    in  lens get set

cimap = CiMap (M.fromList [("a", 1), ("b", 2)])

ex_1 = cimap ^? ix "a" -- Just 1
ex_2 = cimap ^? ix "A" -- Just 1

ex_3 = cimap ^. at "b" -- Just 2
ex_4 = cimap ^. at "B" -- Just 2

ex_5 = cimap & at "a" ?~ 33 -- CiMap (fromList [("a",33),("b",2)])
ex_6 = cimap & at "B" ?~ 14 -- CiMap (fromList [("a",1),("b",14)])
