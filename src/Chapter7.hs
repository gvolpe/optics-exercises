{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- Traversals
module Chapter7 where

import           Control.Applicative            ( ZipList(..)
                                                , liftA2
                                                )
import           Control.Lens
import           Control.Monad.State
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import qualified Data.Map                      as M

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
  let xs =
          ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ( ( Bool
              , String
              )
            , (Bool, String)
            , (Bool, String)
            )
  in  xs & each . filtered fst . traversed . taking 5 traversed %~ toUpper -- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

traversals_7_2_9 =
  let xs =
          ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries")) :: ( ( Bool
              , String
              )
            , (Bool, String)
            , (Bool, String)
            )
  in  xs & each %~ snd -- ("Strawberries","Blueberries","Blackberries")

------------------------------- traversal actions -----------------------------------

actions_1_1 = sequenceAOf _1 (Nothing, "Rosebud") -- Nothing

actions_1_2 = sequenceAOf (traversed . _1) [(['a', 'b'], 1), (['c', 'd'], 2)]
-- [ [('a', 1) ,('c', 2)], [('a', 1) ,('d', 2)], [('b', 1) ,('c', 2)], [('b', 1) ,('d', 2)]]

actions_1_3 = sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]] -- ZipList {getZipList = [[1,3],[2,4]]}

actions_1_4 =
  sequenceAOf (traversed . _2) [('a', ZipList [1, 2]), ('b', ZipList [3, 4])] -- ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}

actions_1_5 =
  let result = traverseOf (beside traversed both)
                          (\n -> modify (+ n) >> get)
                          ([1, 1, 1], (1, 1))
  in  runState result 0 -- (([1,2,3],(4,5)), 5)

-- traverseOf (_1 . traversed) (\c -> [toLower c, toUpper c])("ab", True)
actions_2_1 =
  ("ab", True) & (_1 . traversed) %%~ (\c -> [toLower c, toUpper c])

-- traverseOf (traversed . _1) (\c -> [toLower c, toUpper c]) [('a', True), ('b', False)]
actions_2_2 =
  [('a', True), ('b', False)]
    &   (traversed . _1)
    %%~ (\c -> [toLower c, toUpper c])

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
validateAge = traverseOf
  (user . age)
  (\n -> if n > 0 && n < 150 then Right n else Left "Age out of bounds")

------------------------------- traversal actions -----------------------------------

values :: Applicative f => (a -> f b) -> [a] -> f [b]
values _ []       = pure []
values f (x : xs) = liftA2 (:) (f x) (values f xs)

v1 = ["one", "two", "three"] ^.. values -- ["one", "two", "three"]
v2 = ["one", "two", "three"] & values %~ reverse -- ["eno","owt","eerht"]
v3 = ["one", "two", "three"] & values %~ length -- [3,3,5]

data Transaction = Withdrawal {_amount :: Int} | Deposit {_amount :: Int} deriving Show
makeLenses ''Transaction

newtype BankAccount = BankAccount { _transactions :: [Transaction] } deriving Show
makeLenses ''BankAccount

aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

getAllTransactions = aliceAccount ^.. transactions . traversed -- [Deposit {_amount = 100},Withdrawal {_amount = 20},Withdrawal {_amount = 10}]
getAllTxAmounts = aliceAccount ^.. transactions . traversed . amount -- [100,20,10]

-- deposits :: Traversal' [Transaction] Int
-- deposits :: Traversal [Transaction] [Transaction] Int Int
deposits :: Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits _ []                    = pure []
deposits f (Withdrawal amt : xs) = liftA2 (:) (pure $ Withdrawal amt) (deposits f xs)
deposits f (Deposit amt : xs)    = liftA2 (:) (Deposit <$> f amt) (deposits f xs)

-- Get all the Deposit transaction amounts:
getAllDepositAmounts = [Deposit 10, Withdrawal 20, Deposit 30] ^.. deposits -- [10,30]
multDepositAmountsByTen = [Deposit 10, Withdrawal 20, Deposit 30] & deposits *~ 10 -- [Deposit {_amount = 100},Withdrawal {_amount = 20},Deposit {_amount = 300}]

-- written in terms of other optics
deposits' :: Traversal' [Transaction] Int
deposits' = traversed . filtered isDeposit . amount
 where
  isDeposit :: Transaction -> Bool
  isDeposit (Deposit _) = True
  isDeposit _           = False

-- exercises

--amountT :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
amountT :: Traversal' Transaction Int
amountT f (Deposit x)    = Deposit <$> f x
amountT f (Withdrawal x) = Withdrawal <$> f x

testAmountGet = aliceAccount ^.. transactions . traversed . amountT -- [100,20,10]
testAmountSet = aliceAccount & transactions . traversed . amountT *~ 2 -- BankAccount {_transactions = [Deposit {_amount = 200},Withdrawal {_amount = 40},Withdrawal {_amount = 20}]}

--both' :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
both' :: Traversal (a, a) (b, b) a b
both' f (a1, a2) = (,) <$> f a1 <*> f a2 -- also: liftA2 (,) (f a1) (f a2)

testBothGet = (1, 2) ^.. both' -- [1,2]
testBothSet = (1, 2) & both' *~ 2 -- (2,4)

--transactionDelta :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
transactionDelta :: Traversal' Transaction Int
transactionDelta f (Deposit x)    = Deposit <$> f x
transactionDelta f (Withdrawal x) = Withdrawal . negate <$> f (-x)

testDelta1 = Deposit 10 ^? transactionDelta -- Just 10
testDelta2 = Withdrawal 10 ^? transactionDelta -- Just (-10)
testDelta3 = Deposit 10 & transactionDelta .~ 15 -- Deposit {_amount = 15}
testDelta4 = Withdrawal 10 & transactionDelta .~ (-15) -- Withdrawal {_amount = 15}
testDelta5 = Deposit 10 & transactionDelta +~ 5 -- Deposit {_amount = 15}
testDelta6 = Withdrawal 10 & transactionDelta +~ 5 -- Withdrawal {_amount = 5}

--lefty :: Applicative f => (a -> f a') -> Either a b -> f (Either a' b)
lefty :: Traversal (Either a b) (Either a' b) a a'
lefty _ (Right x) = pure $ Right x
lefty f (Left x)  = Left <$> f x

beside' :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
beside' t1 t2 f (s, s') = (,) <$> (s & t1 %%~ f) <*> (s' & t2 %%~ f)

------------------------------- traversal laws -----------------------------------

laws1 :: [(String, String)]
laws1 = traverseOf both pure ("don't", "touch")

laws1' :: [(String, String)]
laws1' = pure ("don't", "touch")
--laws1 == laws1'

laws2 :: (Int, Int)
laws2 = (0, 0) & both %~ (+10) & both %~ (*10)

laws2' :: (Int, Int)
laws2' = (0, 0) & both %~ (*10) . (+10)
--laws2 == laws2'

-- breaks the second law of consistent focuses
ex1 = "hello" & worded %~ (<> " world") & worded %~ reverse -- "olleh dlrow"
ex2 = "hello" & worded %~ (reverse. (<> " world")) -- "dlrow olleh"

-- breaks the first law of purity
evilFst :: Traversal (Int, a) (Int, b) a b
evilFst f (x, y) = (,) <$> (pure (x + 1)) <*> (f y)

-- the `lined` traversal is unlawful when the update function introduces more newlines
linedIsUnlawful1 = ("hello \n cruel \n world" :: String) & lined %~ (<> "\n") & lined %~ reverse -- " olleh\n\n leurc \n\ndlrow "
linedIsUnlawful2 = ("hello \n cruel \n world" :: String) & lined %~ reverse & lined %~ (<> "\n") -- "\n olleh\n\n leurc \n\ndlrow "
-- linedIsUnlawful1 /= linedIsUnlawful2

------------------------------- traversals: advanced manipulation -----------------------------------

traversals_7_7_1 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1) -- "abc"
traversals_7_7_2 = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _2) -- [1,2,3]

advanced_1 = [1, 2, 3, 4] ^. partsOf (traversed . filtered even) -- [2,4]

advanced_2 = ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed) -- "AarBanCap"

advanced_3 = ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed traversed) -- [1,2,3,4]

advanced_4 = [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40] -- [1,20,3,40]

advanced_5 = ["Aardvark", "Bandicoot", "Capybara"] & partsOf (each . each) .~ "Kangaroo" -- ["Kangaroo","Bandicoot","Capybara"]

advanced_6 = ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant" -- ["Antdvark","Bandicoot","Capybara"]

advanced_7 = M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ \(x:xs) -> xs ++ [x] -- M.fromList [('a','b'),('b','c'),('c','a')]

advanced_8 = ('a', 'b', 'c') & partsOf each %~ reverse -- ('c','b','a')

advanced_9 = [1, 2, 3, 4, 5, 6] & partsOf (taking 3 traversed) %~ reverse -- [3,2,1,4,5,6]

-- changing the type, need a polymorphic optic
advanced_10 = ('a', 'b', 'c') & unsafePartsOf each %~ \xs -> fmap ((,) xs) xs -- (("abc",'a'),("abc",'b'),("abc",'c'))
