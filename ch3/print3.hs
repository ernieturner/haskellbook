-- print3.hs

module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: [Char]
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = concat [hello, " ", world]

--main :: IO ()
--main = do
--    secondGreeting :: String
--    let secondGreeting = concat [hello, " ", world]
--    putStrLn myGreeting
--    putStrLn secondGreeting