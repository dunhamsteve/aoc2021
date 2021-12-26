import System.File
import Data.SortedMap
import Data.String

data Dir = East | South

Show Dir where
    show East = ">"
    show South = "v"

Eq Dir where
    East == East = True
    South == South = True
    _ == _ = False

record Problem where
    constructor P
    grid : SortedMap (Int,Int) Dir
    width : Int
    height : Int

parse : String -> Problem
parse txt =
    let points   = go 0 0 $ unpack txt
        width  = 1 + (foldl max 0 $ map (fst . fst) points)
        height = 1 + (foldl max 0 $ map (snd . fst) points)
        grid = fromList points
    in P grid width height
    where
        go :  Int -> Int -> List Char -> List ((Int,Int),Dir)
        go x y [] = []
        go x y (v::vs) = case v of
            '\n' => go 0 (y+1) vs
            '>'  => ((x,y),East) :: go (x+1) y vs
            'v'  => ((x,y),South)  :: go (x+1) y vs
            _    => go (x+1) y vs
        
Show Problem where
    show p = unlines $ map (\y => pack $ map (getch y) [0..p.width-1]) [0..p.height-1]
        where
            getch : Int -> Int -> Char
            getch y x = case lookup (x,y) p.grid of
                Nothing => '.'
                Just East => '>'
                Just South => 'v' 

stepEast : Problem -> Problem
stepEast prob = { grid $= fromList . map next . toList } prob
    where
        next : ((Int,Int),Dir) -> ((Int,Int),Dir) 
        next kv@((x,y), East) =
            let pt = ((x+1) `mod` prob.width, y) in
            case lookup pt prob.grid of
                Just _ => kv
                Nothing => (pt, East)
        next kv = kv

stepSouth : Problem -> Problem
stepSouth prob = { grid $= fromList . map next . toList } prob
    where
        next : ((Int,Int),Dir) -> ((Int,Int),Dir) 
        next kv@((x,y), South) =
            let pt = (x, (y+1) `mod` prob.height) in
            case lookup pt prob.grid of
                Just _ => kv
                Nothing => (pt, South)
        next kv = kv

step : Problem -> Problem
step = stepSouth . stepEast

run : Problem -> Int -> Int
run prob n = 
    let next = step prob in
    if prob.grid /= next.grid then run next (n + 1)
    else n

doFile : String -> IO ()
doFile fn = do
    putStrLn "** \{fn}"
    Right text <- readFile fn | Left err => printLn err
    let prob = parse text
    -- printLn prob
    printLn $ run prob 1

main = do
    doFile "eg.txt"
    doFile "input.txt"
