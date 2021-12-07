module Main

import System.File
import Data.List
import Data.String
import Data.Nat
import Data.Either
import Debug.Trace

log : Show a => a -> IO ()
log = putStrLn . show

||| Utility to turn a Maybe to Either string
assert : String -> Maybe a -> Either String a
assert err (Just a) = Right a
assert err _ = Left err

opens = unpack "([{<"
close = unpack ")]}>"
pairs = zip opens close

scores : List (Char,Int)
scores = zip close [3,57,1197,25137]

match : Char -> Char -> Bool
match x y = case lookup x pairs of
    Just y' => y == y'
    _ => ?err

score : Char -> Int
score x = case lookup x scores of
    Just s => s
    _ => ?err2

points : List (Char,Int)
points = zip opens [1,2,3,4]

score2 : Int -> List Char -> Int
score2 acc (c :: cs) = case lookup c points of
    Just n => score2 (acc * 5 + n)  cs
    Nothing => ?err3
score2 acc [] = acc

parse : List Char -> List Char -> Either Int Int
parse [] [] = Right 0
parse stk (c::cs) with (c `elem` opens)
    parse stk (c :: cs) | True = parse (c::stk) cs
    parse (s::ss) (c :: cs) | False = 
        if match s c then parse ss cs else Left $ score c
    parse [] (c :: cs) | False = Left $ score c
parse acc [] = Right $ score2 0 acc

main' : String -> IO ()
main' fn = do
    putStrLn fn
    Right text <- readFile fn | Left err => log err
    let result = map (parse [] . unpack) $ lines text
    let points = lefts result
    putStr $ "part1: "
    log $ sum points
    putStr $ "part2: "
    let rest = sort $ rights result
    let n = length rest `div` 2
    let Just value = getAt n rest | Nothing => putStrLn "Can't happen"
    log value
    


