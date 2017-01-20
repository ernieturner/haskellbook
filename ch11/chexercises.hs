module Chexercises where

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

  -- 1. Type constructor
  -- 2. Doggies :: * -> *
  -- 3. Doggies String :: *
  -- 4. Num a => Doggies a
  -- 5. Int a => Doggies Integer
  -- 6. String a => Doggies String
  -- 7. Both?
  -- 8.
  -- 9. DogueDeBordeaux String

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
              | Plane Airline Int
              deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 125
jumbo = Plane TakeYourChancesUnited 370


-- 1. Vehicle
-- 2.

isCar :: Vehicle -> Bool
isCar (Car _ _ ) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- 3.

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. It'll blow up
-- 5.

---- Cardinality
-- 1. 1
-- 2. 3
-- 3. -32768 - 32767 = 65536
-- 4. They're very large
-- 5. 2^8

--- For Example

data Example = MakeExample deriving Show

-- 1. Example, type of Example will error
-- 2. Returns data Example = MakeExample, so yes, you can find out the instances
-- 3.
data Example2 = MakeExample2 Int deriving Show
--- Returned the argument type


tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats2 :: Goats -> Bool
tooManyGoats2 (Goats n) = n > 42

--QUESTION: Not sure I fully understand the benefits of newtype, as this is the same
data GoatsType = GoatsType Int deriving (Eq, Show)
data CowsType = CowsType Int deriving (Eq, Show)
-- newtype is basically an alias I guess

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- In REPL: tooMany (83 :: Int)

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- In REPL: tooMany (Goats 83)

-- Logic Goats

-- in other goats*.hs files

-- Pity the Bool
-- 1. Big Bool | Small Bool =  (1 + 1) + (1 + 1) = 4
-- 2. NumberOrBool = (128 + 128) + (1 + 1) = 258
--    It'll break if you try to create a value outside Int8 range

data Person =
  Person {name :: String,
          age :: Int}
          deriving (Eq, Show)


-- How does your garden grow


data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

--Normal Form
--data GardenNF =
--      Gardenia Gardener |
--      Daisy Gardener |
--      Rose Gardener |
--      Lilac Gardener
--      deriving Show

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

createProgrammer :: OperatingSystem -> ProgrammingLanguage -> Programmer
createProgrammer x y = Programmer {os = x, lang = y}

allProgrammers :: [Programmer]
allProgrammers = map (createProgrammer GnuPlusLinux) allLanguages ++
                 map (createProgrammer OpenBSDPlusNevermindJustBSDStill) allLanguages ++
                 map (createProgrammer Mac) allLanguages ++
                 map (createProgrammer Windows) allLanguages

allProgrammers' :: [Programmer]
allProgrammers' = [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]

correctProgrammerLength = length allProgrammers == length allOperatingSystems * length allLanguages


data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

convert :: Quantum -> Bool
convert No = True
convert Yes = True
convert Both = False

-- The Quad
data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

--eQuad :: Either Quad Quad
--eQuad = Right One
--eQuad = Right Two
--eQuad = Right Three
--etc
-- 8 implementations

--2. prodQuad = 4 * 4 = 16 implementations
--3. functQuad = 4 ^ 4 = 256
--4. prodTBool = 2 * 2 * 2 = 8 implementations
--5. gTwo = 2^2^2 = 16
--6. (2 ^ 4) ^ 4 = 65536

