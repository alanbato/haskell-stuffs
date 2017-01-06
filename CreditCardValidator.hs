module Main where

main :: IO ()
main = putStrLn "Welcome to the Credit Card Validator"

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    | x < 0 = []
    | otherwise = [read [y] :: Integer | y <- show x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (zipWith (*) (cycle [1,2]) n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
    | x <= 9 = x
    | otherwise = sum (toDigits x)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0
