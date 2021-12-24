import Data.SortedMap
import System.File
import Data.List
import Data.Vect
import Debug.Trace

dlog : Show a => String -> a -> a
dlog msg a = trace (msg ++ " " ++ show a) a

Interpolation Int where interpolate = show

-- Board is
-- 0 1 2 3 4 5 6 7 8 9 10 
-- . . . . . . . . . . .
--     .   .   .   .
--     .   .   .   .
--     A   B   C   D
--
-- Holes are indexed by 0,1,2,3, so we have to do 2 + 2*hole to get the position

record Board where
    constructor BD
    top : Vect 11 (Maybe Int)
    holes : Vect 4 (List Int)
    energy : Int
    steps: List String
    depth: Int                 -- for part 2

Eq Board where a == b = a.top == b.top && a.holes == b.holes
Ord Board where a < b = (a.top,a.holes) < (b.top,b.holes)
Show Board where
    show bt = "BD \{show bt.top} \{show bt.holes} \{show bt.energy} \{show bt.steps}"

||| We cheat a _lot_ here by just dropping non-characters and stuffing the rest in the right place.
readProblem : String -> Either String Board
readProblem s = case filter (> 64) $ map (the Int . cast) $ unpack s of
    [a,b,c,d, e,f,g,h] => Right $ BD (replicate 11 Nothing) [[a,e],[b,f],[c,g],[d,h]] 0 [] 2
    _ => Left "Parse Error"

||| Part 2 adds a couple of lines to make the problem harder
readProblem2 : String -> Either String Board
readProblem2 s = case filter (> 64) $ map (the Int . cast) $ unpack s of
    [a,b,c,d, e,f,g,h] => Right $ BD (replicate 11 Nothing) [[a,68,68,e],[b,67,66,f],[c,66,65,g],[d,65,67,h]] 0 [] 4
    _ => Left "Parse Error"

-- Don't know what I should be doing here.  I can't take ranges of Fin, can't
-- increment Fin 4 and get a Fin 7.  They're just a royal pain to work with but
-- needed for Vect...
{n: Nat} -> Cast Int (Fin (S n)) where cast = restrict n . cast

isFree : {auto bd : Board} -> Int -> Bool
isFree n = isNothing $ find (/= 65+n) $ index (cast n) bd.holes
    
blocked : {auto bd : Board} -> Int -> Int -> Bool
blocked hole pos = isJust $ find isJust $ map (\x => index (cast x) bd.top) [2*hole + 2 .. pos]

getMult : Int -> Int
getMult 65 = 1
getMult 66 = 10
getMult 67 = 100
getMult _  = 1000

||| cost to move from hole to position (assuming you've already moved up)
calcCost : Int -> Int -> Int -> Int
calcCost out hole pos = abs $ 2*hole + 2 - pos

||| move from top back into hole
remove : Board -> Int -> Maybe Board
remove bd pos =
    index (cast pos) bd.top >>= \ch =>
    let mult = getMult ch
        hole = ch - 65 -- 0 1 2 3
        down = bd.depth - (cast $ length $ index (cast $ hole) bd.holes)
        cost = down + abs (2 * hole + 2 - pos)
        blocked = isJust $ find isJust $ map (\x => index (cast x) bd.top) $ delete pos [2*hole + 2 .. pos]
    in if not (isFree hole) || blocked then Nothing else 
    let bd' : Board
        bd' = { top $= replaceAt (cast pos) Nothing
              , energy := bd.energy + mult * cost
              , holes $= updateAt (cast $ hole) (ch::)
              , steps $= ("remove \{show $ chr ch} from \{pos}"::)
              } bd                        
    in Just bd'

||| move from hole to top
move : Board -> Int -> Int -> Maybe Board
move bd hole pos =
    if isFree hole then Nothing else
    -- check if blocked
    if isJust $ find isJust $ map (\x => index (cast x) bd.top) [2*hole + 2 .. pos]  then Nothing else
    let content@(ch::rest) = index (cast hole) bd.holes | [] => Nothing in  -- this [] doesn't happen because if isFree above
    let up = 1 + bd.depth - cast (length content)
        cost = getMult ch * (up + abs (2*hole + 2 - pos))
        bd' : Board
        bd' = { top $= replaceAt (cast pos) (Just ch)
              , energy := bd.energy + cost
              , holes $= replaceAt (cast $ hole) rest
              , steps $= ("move \{show $ chr ch} in \{hole} to \{pos}"::) 
              } bd
    in Just bd'
    
{n : Nat} -> Cast (Fin n) Int where cast = cast . finToInteger

||| List of allowed spaces on top row (for iteration)
spaces : List Int
spaces = [0,1,3,5,7,9,10]

||| Best score this board could possibly get, for A* algorithm
estimate : Board -> Int
estimate bd = topSum + holeSum + bd.energy
    where
        calcTop : Int-> Int
        calcTop pos = case index (cast pos) bd.top of
            Nothing => 0
            Just ch => getMult ch * (1 + abs (2*(ch - 65) +2 - pos) )-- don't bother with one vs two?
        
        calcHole : Int -> Int -> Int
        calcHole hole ch = getMult ch * abs (ch - 65 - hole) * 2 + 2 -- ditto

        topSum : Int
        topSum = sum $ map calcTop [0..10]

        holeSum : Int
        holeSum = sum $ concatMap (\i => map (calcHole i) (index (cast i) bd.holes)) [0..3]

data Case = CS Int Board

Eq Case where 
    (CS a b) == CS c d = a == c && b == d

Ord Case where 
    CS a b < CS c d = (a,b) < (c,d)

Show Case where
    show (CS a b) = "CS[\{show a},\{show b}]"

mkCase : Board -> Case
mkCase b = CS (estimate b) b

-- SortedSet doesn't let us pull off the first element
State = SortedMap Case ()

done : Board -> Bool
done bd = (isNothing $ find isJust bd.top) && (isNothing $ find (==False) $ map isFree [0..3])

solve : State -> Maybe Int
solve st = 
    case leftMost st of
        Nothing => Nothing
        Just (k@(CS _ bd), _) =>
            if done bd then trace (show bd) $ Just bd.energy else
            let st' = delete k st
                topTodo, holesTodo : List Board
                topTodo = mapMaybe (remove bd . cast) $ findIndices isJust bd.top
                holesTodo = mapMaybe (uncurry $ move bd) $ [ (hole,pos) | hole <- [0,1,2,3], pos <- spaces ]
                
            in solve $ insertFrom (map (,()) $ map mkCase $ topTodo ++ holesTodo ) st'

doFile : String -> IO ()
doFile fn = do
    putStrLn "** \{fn}"
    Right text <- readFile fn | Left err => printLn err

    putStrLn "P1"
    let Right prob = readProblem text | Left err => printLn err
    printLn $ solve $ singleton (mkCase prob) ()
    
    putStrLn "P2"
    let Right prob = readProblem2 text | Left err => printLn err
    printLn $ solve $ singleton (mkCase prob) ()
    
main : IO ()
main = do
    doFile "eg.txt"
    doFile "input.txt"

