module Hanoi (hanoi) where

type Peg = String
type Move = (Peg, Peg)


hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi count start end temp =
    if count == 1 then [(start, end)]
    else
            hanoi (count - 1) start temp end ++ 
            move ++ 
            hanoi (count - 1) temp end start

    where move = [(start, end)]