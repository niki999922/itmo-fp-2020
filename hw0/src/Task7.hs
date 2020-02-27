module Task7
  ( expressionOne
  , expressionSecond
  , expressionThird
  ) where

import Data.Either (lefts, rights)

{-
  Takes 2 lines and concatenates them, then makes a sheet of pairs with a function and a line,
  applies both uncurry and a (.), also maps this uncurry and (.),
  takes the head from the array and also checks for zero using the (.) operator,
  and we get the final value False.
-}
expressionOne :: Bool
expressionOne = expression
  where
    s1 :: String
    s1 = "Dorian "
    s2 :: String
    s2 = " Grey"
    consStr :: String -> String -> String
    consStr = \x y -> x ++ y
    constStrS1 :: String -> String
    constStrS1 = consStr s1
    strS1S2 :: (String -> String, String)
    strS1S2 = (constStrS1, s2)
    listStrS1S2 :: [(String -> String, String)]
    listStrS1S2 = [strS1S2]
    myId :: (String -> String) -> String -> String
    myId x = x
    myUncurry ::
         ((String -> String) -> String -> String)
      -> (String -> String, String)
      -> String
    myUncurry f = \(x, y) -> f x y
    uncurryId :: ((String -> String), String) -> String
    uncurryId = myUncurry myId
    myMap ::
         ((String -> String, String) -> String)
      -> [(String -> String, String)]
      -> [String]
    myMap _ []     = []
    myMap f (x:xs) = f x : myMap f xs
    mapUncurryId :: [(String -> String, String)] -> [String]
    mapUncurryId = myMap uncurryId
    mapUncurryIdList :: [String]
    mapUncurryIdList = mapUncurryId listStrS1S2
    myHead :: [String] -> String
    myHead _ = ""
    myNull :: String -> Bool
    myNull _ = False
    point_operator :: (b -> c) -> (a -> b) -> (a -> c)
    point_operator f g = \x -> f (g x)
    nullPointHead :: [String] -> Bool
    nullPointHead = point_operator myNull myHead
    otherIdType :: ([String] -> Bool) -> ([String] -> Bool)
    otherIdType x = x
    money :: ([String] -> Bool) -> [String] -> Bool
    money = otherIdType
    expression :: Bool
    expression = money nullPointHead mapUncurryIdList

{-
  We get one number of additions 1 and 2, then the second number by pow 2 and,
  then we make a sheet of them of size 2 of another type of pair,
  then we use a zip pulling out some elements, and we get [(3, 64)].
-}
expressionSecond :: Integral a => [(a, a)]
expressionSecond = zipFull
  where
    firstNumber :: Integral a => a
    firstNumber = onePlusTwo
      where
        one :: Integral a => a
        one = 1
        two :: Integral a => a
        two = 2
        plus :: Integral a => a -> a -> a
        plus a b = a + b
        onePlusTwo :: Integral a => a
        onePlusTwo = plus one two
    secondNumber :: Integral a => a
    secondNumber = twoPowSix
      where
        two :: Num a => a
        two = 2
        six :: Num a => a
        six = 6
        pow :: Integral a => a -> a -> a
        pow a b = a ^ b
        twoPowSix :: Integral a => a
        twoPowSix = pow two six
    list :: Integral a => [Either a a]
    list = createList
      where
        first :: Integral a => Either a a
        first = Left firstNumber
        second :: Integral a => Either a a
        second = Right secondNumber
        createList :: Integral a => [Either a a]
        createList = [first, second]
    leftsX :: Integral a => [Either a a] -> [a]
    leftsX x = lefts x
    leftsXList :: Integral a => [a]
    leftsXList = leftsX list
    rightsXList :: Integral a => [a]
    rightsXList = rightsX list
    rightsX :: Integral a => [Either a a] -> [a]
    rightsX x = rights x
    zipUse :: Integral a => [a] -> [a] -> [(a, a)]
    zipUse = zip
    zipWithFirst :: Integral a => [a] -> [(a, a)]
    zipWithFirst = zipUse leftsXList
    zipFull :: Integral a => [(a, a)]
    zipFull = zipWithFirst rightsXList

{-
  We make 3 numbers 0,2,4, then we add the negation, or, and equalities, and then the implications,
  plus checking through other operators for 2 divisions by 2 and 4 and we get the final expression,
  which return result implication from mod4 on number to mod2 on number.
-}
expressionThird :: Int -> Bool
expressionThird = expression
  where
    number0 :: Int
    number0 = 0
    number2 :: Int
    number2 = 2
    number4 :: Int
    number4 = 4
    notOperation :: Bool -> Bool
    notOperation = not
    modOperation :: Int -> Int -> Int
    modOperation x y = x `mod` y
    orOperation :: Bool -> Bool -> Bool
    orOperation x y = x || y
    equalsOperation :: Int -> Int -> Bool
    equalsOperation x y = x == y
    implicationOperation :: Bool -> Bool -> Bool
    implicationOperation x y = orOperation (notOperation x) y
    isModNumber2 :: Int -> Bool
    isModNumber2 x = equalsOperation (modOperation x number2) number0
    isModNumber4 :: Int -> Bool
    isModNumber4 x = equalsOperation (modOperation x number4) number0
    expression x = implicationOperation (isModNumber2 x) (isModNumber4 x)
