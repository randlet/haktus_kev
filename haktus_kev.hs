import Data.Bits
import PokerArrays


findit xs value low high
    | pivot > value = findit xs value low (mid-1)
    | pivot < value = findit xs value (mid+1) high
    | otherwise	= mid 
    where
    mid = low + (high-low) `div` 2
    pivot = xs!!mid


init_deck :: [Int]
init_deck = [(primes !! j) .|. (shiftL j 8) .|. (shiftR 0x8000 i) .|. (shiftL 1 (16+j)) |  i <- [0..3], j <- [0..12]]


card_rank :: Int -> Int
card_rank x = ((shiftL x 8) .&. 0xF)


find_card :: Int -> Int -> Int -> [Int] -> Int
find_card rank suit start [] = -1
find_card rank suit start (x:xs)
    | ((x .&. suit > 0) && (card_rank(x) == rank)) = start
    | otherwise = find_card rank suit (start + 1) xs


hand_rank :: Int -> Int
hand_rank val
    | val > 6185 = 0
    | val > 3325 = 1
    | val > 2467 = 2
    | val > 1609 = 3
    | val > 1599 = 4
    | val > 322  = 5
    | val > 166  = 6
    | val > 10   = 7
    | otherwise  = 8


eval_5cards :: Int -> Int -> Int -> Int -> Int -> Int
eval_5cards c1 c2 c3 c4 c5
    | (c1 .&. c2 .&. c3 .&. c4 .&. c5 .&. 0xF000) > 0 = flushes !! q
    | s > 0 = s
    | otherwise = values !! findit products ((c1 .&. 0xFF) * (c2 .&. 0xFF) * (c3 .&. 0xFF) * (c4 .&. 0xFF) * (c5 .&. 0xFF)) 0 4887
    where
    q = shiftR (c1 .|. c2 .|. c3 .|. c4 .|. c5) 16
    s = unique5 !! q


all_combos :: [[Int]]
all_combos = [[init_deck!!c1,init_deck!!c2,init_deck!!c3,init_deck!!c4,init_deck!!c5] | c1 <- [0..47], c2 <- [c1+1..48], c3 <- [c2+1..49], c4 <- [c3+1..50], c5 <- [c4+1..51]]
     
all_five :: [[Int]] -> [Int]
all_five [] = []
all_five (x:xs) = eval_5cards c1 c2 c3 c4 c5 : all_five xs
         where
         [c1, c2, c3, c4, c5] = x

ranks :: [Int]
ranks = map hand_rank (all_five all_combos)

rank_counts :: [Int]
rank_counts = map length [[1 | x <- ranks, x == y] | y <- [0..8]]


main = print rank_counts
