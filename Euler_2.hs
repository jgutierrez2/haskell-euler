module Euler_2 where

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

euler2 :: Integer -> Integer
euler2 u = sum $ takeWhile (<=u) evenOnly
    where evenOnly = filter even fibs
