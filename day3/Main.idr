module Main

import System.File
import Data.String
import Data.Nat
import Data.List
import Debug.Trace
import Data.Maybe

log : Show a => a -> IO ()
log = putStrLn . show

middle : Nat -> Nat
middle (S (S n)) = S (middle n)
middle _ = Z

mcbit : List Int -> Int
mcbit cs = fromMaybe 12345 $  getAt (middle (length cs)) (sort cs) 

-- This is a bit of a cheat, we special case singleton list and
-- assume that if len > 1 both bits occur.
lcbit : List Int -> Int
lcbit [x] = x
lcbit x = 1 - (mcbit x)

binToInt : List Int -> Int
binToInt = foldl (\a,x => 2 * a + x) 0

gamma : List (List Int) -> Int
gamma = binToInt  . map (mcbit . sort) . transpose

epsilon : List (List Int) -> Int
epsilon = binToInt  . map (lcbit . sort) . transpose

part1 : List (List Int) -> Int
part1 l = gamma l * epsilon l

filterCol : (List Int -> Int) -> List (List Int) -> Nat -> List (List Int)
filterCol disc dat n = let b = disc <$> traverse (getAt n) dat
    in filter ((b == ) . getAt n) dat

oxygen : List (List Int) -> List (List Int)
oxygen dat = foldl (filterCol mcbit) dat [0..length dat]

co2 : List (List Int) -> List (List Int)
co2 dat = foldl (filterCol lcbit) dat [0..length dat]

part2 : List (List Int) -> Int
part2 dat =
    let [o2] = oxygen dat | _ => 0
        [co2] = co2 dat | _ => 0
    in binToInt o2 * binToInt co2

parse : String -> List (List Int)
parse = map (map ((-48+) . cast) . unpack) . lines

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "* " ++ fn
    Right text <- readFile fn | Left err => log err
    let dat = parse text
    putStr "part1 "
    log $ part1 dat
    putStr "part2 "
    log $ part2 dat

main : IO ()
main = do
    runFile "eg.txt"
    runFile "input.txt"
