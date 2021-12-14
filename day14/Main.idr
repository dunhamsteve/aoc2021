module Main

import System.File
import Data.List
import Data.String.Parser
import Data.String
import Debug.Trace

log : Show a => a -> IO ()
log = putStrLn . show

dlog : Show a => a -> a
dlog a = trace (show a) a

Rule : Type
Rule = ((Char,Char), Char)

%hide State

-- State is a list of pairs and count
-- When we insert, we split into two pairs.
-- The last character is represented by a dummy pair.
-- When we pull off the counts, we just pull off the
-- first character of the pairs, to avoid double counting.

State = List ((Char,Char),Integer)

-- Helper to add a list of (something,count), merging multiple copies of something.
addup : Eq a => Ord a => List (a,Integer) -> List (a,Integer)
addup = go . sort where
    go : List (a,Integer) -> List (a,Integer)
    go ((a,n) :: (b,m) :: rest) = 
        if a == b then go ((a,n+m) :: rest)
        else (a,n) :: go ((b,m) :: rest)
    go x = x

pairs : List Char -> List (Char,Char)
pairs (a :: b :: cs) = (a,b) :: pairs (b :: cs)
pairs [c] = [(c,' ')] -- dummy pair for last char
pairs [] = []

mkState : List Char -> State
mkState = addup . map (,1) . pairs

-- Parsing

word : Parser (List Char)
word = lexeme $ many alphaNum

pRule : Parser Rule
pRule = do
    a <- alphaNum
    b <- lexeme alphaNum
    _ <- lexeme $ string "->"
    c <- lexeme alphaNum
    pure ((a,b),c)

pProblem : Parser (State, List Rule)
pProblem = (,) <$> (mkState <$> word) <*> many pRule

-- Logic

subst' : List Rule -> State -> State
subst' rules (((a,b),n)::rest) = case lookup (a,b) rules of
    Just c => ((a,c),n) :: ((c,b),n) :: subst' rules rest
    Nothing => ((a,b),n) :: subst'  rules rest
subst'  rules [] = []

subst : List Rule -> State -> State
subst rules st = addup $ subst' rules st

swap : (a,b) -> (b,a)
swap (x, y) = (y, x)

expand : State -> List (Char,Integer)
expand (((a,b),n) :: xs) = (a,n) :: expand xs
expand [] = []

calc : List Rule -> State -> Integer -> Integer
calc rules start count =
    let result = foldl (\a,x => subst rules a) start [1..count]
        counts = sort $ map swap $ addup $ expand result
        Just (min,_) = head' counts | Nothing => 0
        Just (max,_) = last' counts | Nothing => 0
    in max - min

runFile : String -> IO ()   
runFile fn = do
    putStrLn $ "* " ++ fn
    Right stuff <- readFile fn | Left err => log err
    -- log stuff
    let Right ((start,rules),_) = parse pProblem stuff | Left err => log err 
    -- log start
    -- log rules
    putStr "part1 "
    log $ calc rules start 10
    putStr"part2 "
    log $ calc rules start 40
    

main : IO ()
main = do
    runFile "eg.txt"
    runFile "input.txt"