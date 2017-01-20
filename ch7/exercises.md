#Grab Bag

1. All the same
2. d 
3.  
   a: addOneIfOdd n = case odd n of
        True -> f n 
        False -> n 
        where f = \n -> n + 1
   
   b: addFive \x -> \y = (if x > y then y else x) + 5
   
   c: mflip f x y = f y x

# Variety Pack

1. a. k :: (x, y) -> x
   b. [Char], different than k1/k3
   c. k1/k3
2. f:: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
   f (a, _, c) (d, _, f) = ((a, d), (c, f))

# Case Practice 

1. functionC x y = 
     case x > y of
      True -> x
      False -> y

2. ifEventAdd2 n = 
     case even n of
       True -> (n + 2)
       False -> n 

3. nums x = 
    case compare x 0 of
     LT -> -1
     GT -> 1
     EQ -> 0

# Artful Dodgy

1. --
2. 11
3. 22
4. 21
5. 12
6. 11
7. 21
8. 21
9. 22
10. 31
11. 23

# Guard Duty

1. Always F
2. No 
3. B 
4. List of anything that supports Eq
5. pal:: Eq a => [a] -> Bool
6. c 
7. Anything deriving from Ord and Eq (GHC infers Num, but is that true?)
8. numbers:: (Eq, Ord) a => a -> a

# Chapter Exercies

## Multiple Choice
1. d 
2. b 
3. d 
4. b 
5. a 

## Let's write code 
1. 









