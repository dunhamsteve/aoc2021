import Debug.Trace

-- die two pawns, board, 1..10
-- die 1..100

log : Show a => a -> IO ()
log = putStrLn . show

eg,input  : (Int,Int)
eg = (4,8)
input = (8,7)

score : Int -> Int
score 0 = 10
score n = n

part1 : (Int,Int) -> Int
part1 pos = let (a,b) = go pos (0,0) 0 1 in a * b
    where
        go : (Int,Int) -> (Int,Int) -> Int -> Int -> (Int,Int)
        go (a,b) (sa,sb) n die =
            let a' = (a + 3*die + 3) `mod` 10
                sa' = sa + score a'
                die' = die + 3
            in if sa' >= 1000 then (sb,n+3)
            else let b' = (b + 3*die' + 3) `mod` 10
                     sb' = sb + score b'
                in if sb' >= 1000 then (sa,n+6)
                else go (a',b') (sa',sb') (n+6) (die' + 3)

-- 1,2,3
-- play to 21

-- At each stage keep "quantum/fuzzy state" - x amount of y

cross : List a -> List b -> List (a,b)
cross as bs = concatMap (\a => map (a,) bs) as
-- sort $ map (uncurry (+)) $ cross [1,2,3] $ map (uncurry (+)) $ cross [1,2,3] [1,2,3]
-- [3, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 9]

run : (Int,Int) -> IO ()
run prob = do
    log prob
    putStrLn "P1 \{show $ part1 prob}"

-- This is a tuple because I don't want to define Eq/Ord.
-- count is last so we can aggregate
Univ : Type
Univ = (Int,Integer,Int,Integer,Integer) -- posA, scoreA, posB, scoreB, count

record State where
    constructor ST
    states : List Univ
    winA : Integer
    winB : Integer

Interpolation Integer where interpolate = show
Interpolation Nat where interpolate = show

Show State where
    show st = "ST #\{length st.states} \{st.winA} \{st.winB} "

combine : Int -> Int -> Int
combine a b = case (a + b) `mod` 10 of 0 => 10; n => n

||| add dice rolls
doadd : List Univ -> (Integer, Int) -> List Univ
doadd states (mult, roll) =
    map (\(pa,sa, pb,sb, cnt) =>
            let pa' = combine pa roll
                sa' = sa + cast pa'
                cnt' = mult * cnt
            in  (pa',sa',pb,sb,cnt')) states

||| coalesce matches (this didn't seem to reduce the per round count,
||| and adds a n log n, so I bailed
coalesce : List Univ -> List Univ
coalesce ss = go [] ss
    where
        go : List Univ -> List Univ -> List Univ
        go [] (x::xs) = go [x] xs
        go xs@((a,b,c,d,cnt)::rest) (x@(a',b',c',d',cnt')::ys) =
            if a==a' && b == b' && c == c' && d == d'
            then go ((a,b,c,d,cnt+cnt')::rest) ys
            else go (x::xs) ys  
        go acc [] = acc

||| peel off winners
final : State -> State
final st = go st.winA [] st.states 
    where
        go : Integer -> List Univ -> List Univ -> State
        go acc rval ((pa,sa, pb,sb, cnt) :: us) =
            if sa >= 21 then go (acc+cnt) rval us
            else go acc ((pb,sb, pa,sa, cnt)::rval) us -- flip a/b as we accumulate, so we can run the next turn
        go acc rval [] = ST rval st.winB acc           -- flip the wins too

rolls : List (Integer,Int)
rolls = [(1,3),(3,4),(6,5),(7,6),(6,7),(3,8),(1,9)]

step : State -> State
step st@(ST states winA winB) =
    let states' = concatMap (doadd states) rolls
    in  final $ ST states' winA winB

mkState : (Int,Int) -> State
mkState (pa,pb)= ST [(pa,0,pb,0,1)] 0 0

run2 : State -> (Integer,Integer)
run2 st = case step st of
    (ST [] winA winB) => (winA, winB)
    st => run2 $ trace (show st) st

main = do
    putStrLn "eg"
    run eg
    putStrLn "P2 \{show $ run2 $ mkState eg}"

    putStrLn "input"
    run input
    putStrLn "P2 \{show $ run2 $ mkState input}"