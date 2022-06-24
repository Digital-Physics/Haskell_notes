--Need to think more about this IO and what's up with putStrLn
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
Type signatures show up with IDE plug-in.
So some lines of code don't include them.
I probably should put them in for viewing in GitHub and for clarity sake 
even though the Type inference works well.
-}

--Num Typeclass:
--https://en.wikibooks.org/wiki/Haskell/Type_basics_II
--Num includes Int (small int type), Integer(unbounded int type), Double(floating type)
double :: Num a => a -> a 
double x = x + x

-- Currying (passing arguments one at a time and returning intermediate functions)
-- https://wiki.haskell.org/Currying
-- function_name input_argument_1 input_argument_2
jdkAdd :: Num a => a -> a -> a
jdkAdd x y = x + y 

--parentheses were needed here for the input/output Types to work
quadruple x = double (double x)
 
factorial n = product [2..n]

average ns = sum ns `div` length ns

otherAverage ns = div (sum ns) (length ns)

-- f(x) = (x+1)^2
poly x = let
    y = x + 1
    in y*y

sumEvenOdds xs = sums (incr (evens xs))
    where
        sums xs = foldr (+) 0 xs
        incr xs = map (+1) xs
        evens xs = filter (\x -> x `mod` 2 == 0) xs


x = 8

y = True

z = x + x

--Char are Unicode characters
--strings are lists of characters
a = "b"

b = ["b", "s"]

c = ("b", 7)

d = ['a','b','c'] 
e = [True, False, False]
f = (True, False)
g = ("a",[8,9,10],True)

h = "bob"

i = ['b','o','b']

j x y = x + y

k :: Integer -> Integer -> Integer
k x y = x + y

first xs = head xs

--a String is a List of Chars, I think
tester :: [Char]
tester = "hello" ++ "goodbye"

--type signatures are helpful for understanding
first2 :: (a,b) -> a
first2 (x,y) = x

--Curried 
--arguments passed one by one by default in Haskell (Curry)
--same as Int-> Int -> Int
add :: Int -> (Int -> Int)
add x y = x + y

--define a function in terms of another function
add5 :: Int -> Int
add5 = add 5

map_test :: [Int] -> [Int]
map_test xs = map (+1) xs

--uncrried functions prevent currying
--they do this by grouping all function arguments into a tuple
uncurriedAdd :: (Int,Int) -> Int
uncurriedAdd (x,y) = x+y

--we don't need to put the argument in; it's implied
--mapTest2 is just a function, a higher order function
mapTest2 :: [Int] -> [Int]
mapTest2 = map add5

--polymorphic functions can handle multiple types
--this function returns the fist element of the tuple, regarless of type
--if you want to restrict to a particular type, then specify it in the type signature
--lower case letters represent a generic type variable. 
--functions that ca be implemented w/ same formula for any type are "parametrically polymorphic"
myFst :: (a,b) -> a
myFst (x,y) = x

--Functions to Bool are "predicates"
--Note: "Int -> Bool" type signature is ok, but Integral -> Bool needs to be bound to a?
--Integral covers Int and Integer (second of which is unbounded in size, I think.) they both have mod methods, i guess.
isEvenJdk :: Integral a => a -> Bool
isEvenJdk x = if (x `mod` 2 == 0) then True else False

--map and fst built in already
-- :t map
--map :: (a -> b) -> [a] -> [b]
-- :t fst
--fst :: (m, n) -> m
--type signature type variables just need to be locally consistent
myMap = map fst

--with explicit argument added:
--lastElem xs = (head.reverse) xs
lastElem :: [a] -> a
lastElem = head.reverse

--parenthesis can build up; $ can help
--foldr (+) 0 (map (+2) (reverse (init (x:xs))))
test_func (x:xs) = foldr (+) 0 $ map (+2) $ reverse $ init $ x:xs

--Type Classes help create polymorphic functions
--Functions with different implementation/methods depending on input type
 --the Prelude Eq type class already implement only has type signatures
 --we will make a slight variation in the equivalence function def/symbols
 --note on symbols vs letters/nums: it doesn't look like you could define it as (==2) or (==jdk)
class EqJdk a where
    (==<>) :: a -> a -> Bool
    (/=<>) :: a -> a -> Bool

--in order to use a function from a type class you need to make an instance
--instance/definition for Bool type input shown below
--Int and Float would have different, and more complicated defintions since they are infinte classes
--Eq is a predefined class for Bool, Char, String, Int, Integer, Foat
--Other predefined classes: Show, Read, Eq => Ord (binding to ordering operator), etc.
instance EqJdk Bool where
    x ==<> y = if x then (if y then True else False)
                  else (if y then False else True)
    x /=<> y = not (x == y)

--overloaded polymorphic functions have class constraints
--for example, on a polymorphic sum function needs to have an instance of Num class 
--instead of this, which isn't very generally defined
--add :: Int -> Int -> Int
-- the (+) function is a member of the Num class, so bind (=>) "Num a" to a
--add :: Num a => a -> a -> a

--infix operators go in brackets 
--infix notation is when the operator is used in between two operators (e.g. x/y)
--to get more information about and a class or operator:
-- :info (/)
-- :info Fractional 
-- :t (/) 

-- Fractional is defined for Float and Double
-- "divide 1 3 :: Float" vs "divide 1 3 :: Double" give two different answers
divide :: Fractional a => a -> a -> a
divide x y = x / y

--the Integral type is bound to the function variable a
--Haskell has two Integral type instances (Int and Integer) (similar to Float and Double)
--Int is finite in size and allows your program to run faster
--This division returns the integer part
divide2 :: Integral a => a -> a -> a
divide2 x y = x `div` y

--Pattern Matching
--List Patterns
--[1,2,3] is "syntactic sugar" for 1:(2:(3:[])) using : the "cons" operator
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

--safeHead :: [a] -> a
--safeHead [] = [] --not correct because of the type signature, but is there a None type? or "Maybe"?
--safeHead (x:xs) = x

--Lambda expressions (nameless functions that you use once, on the fly, based on Lambda Calculus)
--Lambda expressions are useful when defining higher order functions that return functions as a result
--def 1
addJdk x y = x + y
--def 2 (using lambda expression)
addJdk2 = \x -> (\y -> x + y)

--Map and Lambda Expressions 
--If you are only using the Map function (parameterized by whatever function) once, why not lambda
add3s xs = map (\x -> x+3) xs

--Recursion
--In order to define our map, we recurse through its input list elements and pattern match
mapJdk :: (a->b) -> [a] -> [b]
mapJdk f [] = []
mapJdk f (x:xs) = f x : mapJdk f xs

-- example of recursive call:
-- mapJdk (\x -> x + 1) [10..12]
-- or
-- mapJdk (1+) [10..12] 
--
-- remember [10..12] = [10,11,12] = (10:11:12:[])
--
-- recursively executed steps:
-- (1+10):(mapJdk (1+) [11,12])
-- (1+10):(1+11):(mapJdk (1+) [12])
-- (1+10):(1+11):(1+12):(mapJdk (1+) [])
-- (1+10):(1+11):(1+12):[]
-- [11,12,13]

-- Category Theory (composition of arrows and identity)
-- (.) composition 
-- Note: Composition is the B Combinator (a rewrite rule) in Combinatory Logic w/ S K)
-- (B f g x) = (f(g x))
-- B = (S (K S) K)
--Note: Composition is associative
-- Q: Do we want to define composition on generic types? (lower case variables in type signature) 
-- A: No, because they need to have this matching relationship. (I think)
-- f::A->B
-- g::B->C
-- h::C->D
-- h.(g.f) == (h.g).f == h.g.f (Note: equality between functions not defined in Haskell)
--
-- Identity arrow 
-- Here the type signature shows a generic type
-- id :: a -> a
-- id x = x
--
-- Why is coposition in Haskell important?
-- Because we can only keep in our head ~7 in our head
-- Because it leads to simple, elegant, readable code
-- Easier to read than C++

-- Challenge: implement all possible functions that are from Bool -> Bool
alwaysTrue :: Bool -> Bool
alwaysTrue x = True

alwaysFalse :: Bool -> Bool
alwaysFalse x = False

notJdk :: Bool -> Bool
notJdk x = not x

idJdk :: Bool -> Bool
idJdk x = x

-- challenge: implement all possible functions from [1,2,3] to Bool
-- The number of possible functions = |codomain or target| raised to the |domain or source| (|x| is size of x)
-- A function needs an output for every possible input, but the input domain doesn't need to cover "onto" the codomain
--firstOf8 :: [1,2,3] -> Bool  (incorrect syntax)
--set = [1,2,3] doesn't work; you can't just create things without functions
--data Int = f1 | f2 | f3
--firstOfEight :: data -> Bool
--firstOfEight 1 = True
--firstOfEight 2 = False
--firstOfEight 3 = True

--a function that takes no arguments can never be called
--at least one thing is needed to make it run. let's use a singleton type, or unit... () 
-- f2 is a different representation for 2
f2 :: () -> Int
f2 () = 2 --pattern matched

f1 :: () -> Int
f1 () = 1

--"terminal object", i think
--exactly one function like this for any input Type
--the underscore or wildcard can be used when the variable isn't needed, like for loops where you don't need the index
unitJdk :: a -> ()
unitJdk _ = ()

--The type Void represents falsity
--From a falsity, anything follows (but at least incompleteness doesn't apply ;)
--absurd :: Void -> a

--"memoize" higher order function
--saves the results in cache for quicker computing after first time run
--(see "Category Theory for Programmers" by Bartosz Milewski)
--memoizeJdk :: (a -> b) -> (a->b)
--memoizeJdk f = if f a 

--Monoid
--Nuetral/identity element called mempty and a binary operation called mappend
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m

--Furnction arguments are sometimes called points
--Function equality wtihout specifying the arguments is described as point-free
--Point-Free equations often involve composition of functions, symboized by a . (a little confusing)


-- scrap tests below******
pairwiseSum :: Num a => [a] -> [a] -> [a]
pairwiseSum = zipWith (+)

--list comprehension
pow3list = [3^n | n <- [1..10]]


--types in Haskell:

--Int : -2^63 to 2^63-1
maxInt = maxBound :: Int
--9223372036854775807
minInt = minBound :: Int
---9223372036854775808

--Integer : unbounded 
--Double : double-precision floating point > single-precision Float type
bigFloat = 0.99999999 + 0.0005
problematicFloat = 0.99999999999999999999 + 0.0005

--Bool : True False
--Char : '
--Tuple : 

sumOfNums = sum [1..1000]

--type:
-- t: sqrt
-- to see:
--sqrt :: Floating a => a -> a
num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

otherSqrtOf9 = sqrt 9

valOfPi = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
otherSquared9 = 9^2
round9 = round 9.999
ceiling9 = ceiling 9.7
floor9 = floor 9.8
sin9 = sin 9

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

matrix :: [[Integer]]
matrix = [[x*y | y <- [1..10]]| x <- [1..10]]

intList = [1..10] :: [Int]

matrix2 ::  Num a => [a] -> [a] -> [[a]]
matrix2 x y = [[x*y| x <- x]| y <- y]

-- another change test








