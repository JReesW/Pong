module Maths where


countDigits :: Int -> Int
countDigits n | n >= 10   = 1 + countDigits (n `div` 10)
              | otherwise = 1


digits :: Int -> [Int]
digits 0 = [0]
digits n = digits_ n []
    where digits_ 0 ls = ls
          digits_ n ls = digits_ (n `div` 10) (n `mod` 10 : ls)


leftOffset :: Int -> Float
leftOffset n = 50 * (fromIntegral (countDigits n) - 1)
