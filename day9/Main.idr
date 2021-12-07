module Main

import System.File
import Data.String
import Data.List
import Data.Vect
import Data.SortedMap

log : Show a => a -> IO ()
log = putStrLn . show

Grid = SortedMap (Int,Int) Int

record Problem where
    constructor P
    w : Int
    h : Int
    grid : Grid

Show Problem where
    show (P w h grid) = "Prob[" ++ show w ++ "," ++ show h ++ "," ++ show grid ++ "]"

parse : String -> Problem
parse t = parse' empty 0 0 $ unpack t
    where parse' : Grid  -> Int -> Int -> List Char -> Problem
          parse' res x y ('\n' :: cs) = parse' res 0 (y + 1) cs
          parse' res x y (c :: cs) = let res' = insert (x,y) (the Int $ cast c - 48) res
            in parse' res' (x+1) y cs
          parse' res w h [] = P w (h+1) res

adj : (Int,Int) -> List (Int,Int)
adj (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

solve : Problem -> List ((Int,Int),Int)
solve (P w h grid) =
    let get : (Int,Int) -> Maybe Int
        get pt = lookup pt grid
        risk : (Int,Int) -> Bool
        risk (x,y) = 
            let Just value = get (x,y) | _ => False
                neighbors = mapMaybe get $ adj (x,y)
            in case find (\n => value >= n) neighbors of
                Just _  => False
                Nothing => True
    in filter (risk . fst) $ Data.SortedMap.toList grid

fill : Grid -> Int -> List (Int, Int) -> Int
fill grid acc [] = acc
fill grid acc (pt :: pts) = 
    case lookup pt grid of
        Just 9  => fill grid acc pts
        Nothing => fill grid acc pts
        Just v  => fill (insert pt 9 grid) (acc + 1) (pts ++ adj pt)

main' : String -> IO ()
main' fn = do
    putStrLn fn
    Right text <- readFile fn
        | Left err => log err
    let problem = parse text 
    let solution = solve problem
    putStr "Part1: "
    log $ sum . map (the Int 1 + ) . map snd $ solution
    putStr "Part2: "
    let basins = map (\pt => fill problem.grid 0 [pt]) $ map fst solution
    log $ foldl (*) 1 $ take 3 $ reverse $ sort basins
    
main : IO ()
main = do
    main' "eg.txt"
    main' "input.txt"