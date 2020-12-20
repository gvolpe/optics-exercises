{-# LANGUAGE TemplateHaskell #-}

-- Traversals
module Chapter7 where

import           Control.Applicative            ( ZipList(..) )
import           Control.Lens
import           Control.Monad.State
import           Data.Char                      ( toLower, toUpper )

traversals_7_1_1 = "You get a Fold"
traversals_7_1_2 = "Lenses (and traversals, of course)"
traversals_7_1_3 = "Both Lenses & Traversals (and folds, of course)"

traversals_7_2_1 = ("Jurassic", "Park") & each .~ "N/A" -- ("N/A","N/A") -- could also use `both`

traversals_7_2_2 =
  let xs = ("Jurassic", "Park") :: (String, String)
  in  xs & both . traversed .~ 'x' -- ("xxxxxxxx","xxxx") -- could also use `both . each`:

traversals_7_2_3 =
  let xs = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
  in  xs & beside id traversed %~ take 3 -- ("Mal",["Kay","Ina","Jay"])

traversals_7_2_4 =
  let xs = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
  in  xs & _2 . element 1 .~ "River" -- ("Malcolm",["Kaylee","River","Jayne"])

traversals_7_2_5 =
  let xs = ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
  in  xs & traversed . elementOf worded 1 . traversed .~ 'x' -- ["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]

traversals_7_2_6 = ((1, 2), (3, 4)) & both . both +~ 1 -- ((2, 3), (4, 5)) -- could also be `each . each` or `both . each`

traversals_7_2_7 = (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1 -- (2, (3, [4, 5]))

-- it would also work if we replace the `traversed` after the filter by `_2`
traversals_7_2_8 =
  let xs = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ((Bool, String), (Bool, String), (Bool, String))
  in  xs & each . filtered fst . traversed . taking 5 traversed %~ toUpper -- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

traversals_7_2_9 =
  let xs = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ((Bool, String), (Bool, String), (Bool, String))
  in  xs & each %~ snd -- ("Strawberries","Blueberries","Blackberries")

------------------------------- traversal actions -----------------------------------

actions_1_1 = sequenceAOf _1 (Nothing, "Rosebud") -- Nothing

actions_1_2 = sequenceAOf (traversed . _1) [(['a', 'b'], 1), (['c', 'd'], 2)]
-- [ [('a', 1) ,('c', 2)], [('a', 1) ,('d', 2)], [('b', 1) ,('c', 2)], [('b', 1) ,('d', 2)]]

actions_1_3 = sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]] -- ZipList {getZipList = [[1,3],[2,4]]}

actions_1_4 = sequenceAOf (traversed . _2) [('a', ZipList [1,2]), ('b', ZipList [3,4])] -- ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}

actions_1_5 =
  let result = traverseOf (beside traversed both) (\n -> modify (+n) >> get) ([1, 1, 1], (1, 1))
  in  runState result 0 -- (([1,2,3],(4,5)), 5)

-- traverseOf (_1 . traversed) (\c -> [toLower c, toUpper c])("ab", True)
actions_2_1 = ("ab", True) & (_1 . traversed) %%~ (\c -> [toLower c, toUpper c])

-- traverseOf (traversed . _1) (\c -> [toLower c, toUpper c]) [('a', True), ('b', False)]
actions_2_2 = [('a', True), ('b', False)] & (traversed . _1) %%~ (\c -> [toLower c, toUpper c])

data User = User
  { _name :: String
  , _age :: Int
  } deriving Show
makeLenses ''User

data Account = Account
  { _id :: String
  , _user :: User
  } deriving Show
makeLenses ''Account

validateAge :: Account -> Either String Account
validateAge = traverseOf (user . age) (\n -> if n > 0 && n < 150 then Right n else Left "Age out of bounds")
