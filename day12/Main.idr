module Main

import System.File
import Data.String
import Data.List1
import Data.List
import Data.SortedMap
import Data.Maybe

log : Show a => a -> IO ()
log = putStrLn . show

-- File list graph edges

parseEdge : String -> Maybe (String,String)
parseEdge line = case split (=='-') line of
    a ::: b :: [] => Just (a,b)
    _ => Nothing

parseFile : String -> Maybe (List (String,String))
parseFile = traverse parseEdge . lines

-- We'll want adjacent edges

AMap = SortedMap String (List String)

lookup' : AMap -> String -> List String
lookup' adj k = fromMaybe [] (lookup k adj)


addEdge : AMap -> (String,String) -> AMap
addEdge adj (a,b) =
    insert a (b :: lookup' adj a) $
    insert b (a :: lookup' adj b) adj

mkAMap : List (String,String) -> AMap
mkAMap = foldl addEdge empty


-- Part 1

-- we want to find all paths that visit a given small cave at most once.

-- Part 2

-- we can visit one small cave twice (not start/end though)

isSmall : String -> Bool
isSmall s = s == toLower s

-- This was written to cover both part1 and part2 and the original part1 solution dropped.

countPaths : AMap -> Bool -> String -> List String -> Int
countPaths adj _ "end" _ = 1
countPaths adj sm p ps with (isSmall p && elem p ps)
    countPaths _ False "start" _ | True = 0
    countPaths _ True p ps | True = 0
    countPaths adj r p ps | r' =
        foldl (\a,n => a + countPaths adj (r || r') n (p::ps)) 0 $ lookup' adj p

main' : String -> IO ()
main' fn = do
    putStrLn $ "* " ++ fn
    Right text <- readFile fn | Left err => log err
    let Just edges = parseFile text | Nothing => putStrLn $ "parse error for " ++ fn
    -- log edges
    let amap = mkAMap edges
    -- log amap
    putStr "p1: "
    log $ countPaths amap True "start" []
    putStr "p2: "
    log $ countPaths amap False "start" []

main : IO ()
main = do
    main' "eg.txt"
    main' "eg2.txt"
    main' "eg3.txt"
    main' "input.txt"
    