module Lib (validate) where

toDigits n = reverse (toDigitsRev n)

toDigitsRev n
    | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
    | otherwise = []

doubleEveryOtherRev (x:y:xs) = 
    x : 2*y : doubleEveryOtherRev xs
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev [] = []

sumDigits = sum . map (sum . toDigits)

validate num = (sumDigits . doubleEveryOtherRev . toDigitsRev $ num) `mod` 10 == 0
