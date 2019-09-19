module LearnParsers where

import Text.Trifecta

import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

-- one'' = string "123" >> string eof

onePrime = char '1' >> char '2' >> char '3' >> eof

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser [Char] -> IO ()
testParse' p = print $ parseString p mempty "123"

testParseEmpty :: Parser () -> IO ()
testParseEmpty p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "onePrime:"
    testParseEmpty onePrime
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'