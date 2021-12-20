
import System.File
import Data.SortedSet
import Data.String
import Data.List
import Data.Vect

%hide Builtin.DPair.DPair.fst
%hide Prelude.toList

-- Infinite Cellular Automata

Alg = Vect 512 Char

||| The state of our automata. The actual input (but not the example) flips the
||| entire universe every other step, so if 0 is mapped to 1, we flip our representation
||| every step.
record State where
    constructor ST
    inv : Bool -- does bits represent set values or unset values
    bits : SortedSet (Int,Int)

parseState : List Char -> State
parseState = go empty 0 0
    where
        go : List (Int,Int) -> Int -> Int -> List Char -> State
        go acc x y (c::cs) = case c of
            '\n' => go acc 0 (y+1) cs
            '#'  => go ((x,y) :: acc) (x+1) y cs
            _    => go acc (x+1) y cs
        go acc _ _ [] = ST False $ fromList acc
            
contains : (Int,Int) -> State -> Bool
contains pt@(x,y) st = st.inv /= contains pt st.bits

parse : String -> Maybe (String,State)
parse text =
    let alg :: rest = lines text | _ => Nothing
    in Just (alg, parseState . unpack . unlines . filter (/= "") $ rest)

Show State where
    show st =
        let pts = toList st.bits
            minX = foldl min 0 (map fst pts)
            maxX = foldl max 0 (map fst pts)
            minY = foldl min 0 (map snd pts)
            maxY = foldl max 0 (map snd pts)
        in unlines $ show st.inv :: map (\y => pack $ map (\x => if contains (x,y) st.bits then '#' else '.') [minX .. maxX]) [minY..maxY] 

step : Alg -> State -> State
step alg st =
    let flippy = index 0 alg == '#'
        inv' = flippy && not st.inv
        pts  = toList st.bits
        minX = foldl min 0 (map fst pts)
        maxX = foldl max 0 (map fst pts)
        minY = foldl min 0 (map snd pts)
        maxY = foldl max 0 (map snd pts)

        get : (Int,Int) -> Int
        get pt = if contains pt st then 1 else 0

        -- Window value for a point
        window : (Int,Int) -> Int
        window (x,y)= foldl (\a,x => 2*a+x) 0 $ map get $ the (List _)
            [ (x-1, y-1), (x, y-1), (x+1, y-1)
            , (x-1, y),   (x, y),   (x+1, y)
            , (x-1, y+1), (x, y+1), (x+1, y+1)
            ]
        
        -- Next value for a point, flipping if necessary
        next : (Int,Int) -> Maybe (Int,Int)
        next pt = 
            if ('#' == index (restrict 511 $ cast $ window pt) alg) /= inv'
            then Just pt else Nothing

    in ST inv' $ fromList $ concatMap (\y => mapMaybe (next . (,y)) [minX-3 .. maxX+3]) [minY - 3 .. maxY+3]
    

run : Alg -> State -> Nat -> State
run _ st Z = st
run alg st (S n) = run alg (step alg st) n

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "* " ++ fn
    Right text <- readFile fn | Left err => print err
    let Just (alg,st) = parse text | Nothing => putStrLn "Failed to parse state"
    let Just alg = toVect 512 $ unpack alg | Nothing => putStrLn "malformed spec"
    -- print st
    -- print $ run alg st 2
    putStrLn $ "P1 = " ++ (show $ length $ SortedSet.toList $ .bits $ run alg st 2)
    putStrLn $ "P2 = " ++ (show $ length $ SortedSet.toList $ .bits $ run alg st 50)
    
main : IO ()
main = do
    runFile "eg.txt"
    runFile "input.txt"
    