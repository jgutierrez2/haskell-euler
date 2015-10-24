module Euler_3 where

euler3 :: Integer -> Integer
euler3 i = lpf i 2
    where lpf j u
            | j == u = j
            | mod j u == 0 = lpf (div j u) u
            | otherwise = lpf j (u + 1)
