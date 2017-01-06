module Main where

main :: IO()
main = putStrLn "Welcome to the Hanoi solver"

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n x y z
    | n <= 0 = []
    | otherwise = hanoi (n-1) x z y ++ [(x,y)] ++ hanoi (n-1) z y x
