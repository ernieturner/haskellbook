-- exercises

-- Scope
-- 1. Yes
-- 2. No
-- 3. No
-- 4. Yes

-- Syntax
-- 1. Yes, compiles
-- 2. No, needs double quotes "<3" ++ "Haskell"
-- 3. Yes, compiles

-- Chapter Exercises

-- Syntax
-- 1
  -- a: Correct
  -- b: Correct
  -- c: Correct
  -- d: Missing last quote
  -- e: Wrong order ("hello" || 4)
  -- f: Correct
  -- g: Number shouldn't be in quotes (take 4 "lovely")
  -- h: Correct

  -- 2
    -- a -> d
    -- b -> c
    -- c -> e
    -- d -> a
    -- e -> b

-- Binding Functions
-- 1
    -- a: "Curry is awesome" ++ "!"
    -- b: "Curry is awesome" !! 4
    -- c: drop 9 "Curry is awesome!"

-- 2
    -- a:

exclaim :: String
exclaim x = x ++ "!"

    -- b:

fourthChar :: String
fourthChar x = [x !! 4]

    -- c:

removeFirstNine :: String
removeFirstNine x = drop 9 x

-- 3
thirdLetter :: Char
thirdLetter x = x !! 4

-- 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" || x

-- 5
rvrs :: String -> IO ()
rvrs input = putStrLn y
    where
        firstWord = take 5 input
        secondWord = drop 9 input
        y = concat [secondWord, " is ", firstWord]

-- 6


