module Euler_1 where

sumTo :: Integer -> Integer
sumTo u = (1 + u) * div u 2 + adj
    where adj = if even u then 0 else div u 2 + 1

sumMultTo :: Integer -> Integer -> Integer
sumMultTo a u = a * sumTo x
    where x = div u a

euler1 :: Integer -> Integer -> Integer -> Integer
euler1 a b u = x + y - z
    where [x,y,z] = map (flip sumMultTo u) [a, b, a * b]
