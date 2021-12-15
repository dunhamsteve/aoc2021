module Main

import System.File
import Data.String
import Data.List
import Data.Vect
import Data.SortedMap
import Debug.Trace
import Data.Buffer

log : Show a => a -> IO ()
log = putStrLn . show

dlog : Show a => a -> a
dlog a = trace (show a) a

Point : Type
Point = (Int,Int)

Grid = (Int,Int) -> Maybe Int

part1grid : SortedMap Point Int -> Grid
part1grid grid pt = lookup pt grid

domod : Int -> Int
domod x = case x `mod` 9 of 
    0 => 9
    _ =>  x `mod` 9

part2grid : SortedMap Point Int -> Point -> (Int,Int) -> Maybe Int
part2grid grid (mx, my) (x,y) =
    let mx1 = mx + 1
        my1 = my + 1
    in if x < 0 || y < 0 || x >= mx1*5 || y >= my1 * 5 then Nothing
    else let Just v = lookup (x `mod` mx1, y `mod` my1) grid | Nothing => ?shouldntHappen
      in Just  $ domod $ v + (x `div` mx1) + (y `div` my1)

record State where
    constructor ST
    scores : SortedMap Point (Int,Bool)
    todo : SortedMap (Int, Point) ()

updatePoint : State -> (Int, Point) -> Int -> State
updatePoint st old@(sc,pt) new =
    ST (insert  pt (new,False) st.scores)
       (insert (new,pt) () (delete old st.todo))
       
done : State -> (Int,Point) -> State
done st old@(score,pt) =
    ST (insert pt (score,True) st.scores)
       (delete old st.todo)

initial : State
initial = ST (fromList [((0,0),(0,False))]) (fromList [( (0,(0,0)), () )])

minNode : State -> Maybe (Int,Point)
minNode st = fst <$> leftMost st.todo

parse : String -> SortedMap Point Int
parse t = parse' empty 0 0 $ unpack t
    where parse' : SortedMap Point Int  -> Int -> Int -> List Char -> SortedMap Point Int
          parse' res x y ('\n' :: cs) = parse' res 0 (y + 1) cs
          parse' res x y (c :: cs) = let res' = insert (x,y) (cast c - 48) res
            in parse' res' (x+1) y cs
          parse' res _ _ [] = res

adj : (Int,Int) -> List (Int,Int)
adj (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

update : Grid -> Int -> State -> (Int,Int) -> State
update grid pathCost state pt = 
    let Just cost = grid pt | Nothing => state
    in case lookup pt state.scores of
        Just (score, visited) => if score > pathCost + cost 
            then updatePoint state (score,pt) (pathCost + cost)
            else state
        Nothing => updatePoint state (0,pt) (pathCost + cost)

visit : Grid -> State -> (Int,Int) -> State
visit grid state pt = case lookup pt state.scores of
    -- review - what if it was already scored.
    Just (path,visit) => foldl (update grid path) (done state (path,pt)) (adj pt)
    Nothing => ?state

unvisited : (Point,(Int,Bool)) -> Bool
unvisited (_,(_,x)) = not x

findPath : (Int,Int) -> Grid -> State -> Maybe (Int,Bool)
findPath dest grid st = case minNode st of
    Just (_,pt)   => if pt == dest then findPath dest grid $ visit grid st pt --lookup dest st
                   else findPath dest grid $ visit grid st pt
    Nothing   => lookup dest st.scores

bigger : (Int,Int) -> (Int,Int)
bigger (x,y) = (x * 5 + 4, y * 5 + 4)

dump : Grid -> Point -> IO ()
dump grid (x,y) =
    foldlM (\_,y => 
        foldlM (\_,x => 
            let Just c = grid (x,y) | Nothing => pure ()
            in putStr (singleton $ cast $ c + 48)
        ) () [0..x] >> putStrLn ""
    ) () [0..y]

dump2 : Grid -> State -> Point -> IO ()
dump2 grid st (x,y) =
    foldlM (\_,y => 
        foldlM (\_,x => case lookup (x,y) st.scores of
            Just (cost,visit) => putStr $ (show cost) ++ " " 
            Nothing => putStr "- "
        ) () [0..x] >> putStrLn ""
    ) () [0..y]

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "** " ++ fn
    Right text <- readFile fn | Left err => log err
    let mtx = parse text
    let goal = foldl max (0,0) $ map fst $ Data.SortedMap.toList mtx
    log $ findPath goal (part1grid mtx) $ initial
    log (bigger goal)
    let p2 = part2grid mtx goal
    log $ p2 (bigger goal)
    log $ p2 (10,0)
    putStr "P2 "
    log $ findPath (bigger goal) (part2grid mtx goal) $ initial
    

main : IO ()
main = do
    runFile "eg.txt"
    runFile "input.txt"
