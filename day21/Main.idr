import Debug.Trace

-- die two pawns, board, 1..10
-- die 1..100


Interpolation Integer where interpolate = show
Interpolation Int where interpolate = show
Interpolation Nat where interpolate = show

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

record Univ where
    constructor U
    pos : Int
    score : Int
    count : Integer

Show Univ where show u = "U \{u.pos} \{u.score} \{u.count}"

record Player where
    constructor P
    states : List Univ
    wins : Integer

Show Player where show p = "P[#\{length p.states},\{p.wins}]"

State : Type
State = (Player,Player)


combine : Int -> Int -> Int
combine a b = case (a + b) `mod` 10 of 0 => 10; n => n

||| add dice rolls
doadd : List Univ -> (Integer, Int) -> List Univ
doadd states (mult, roll) = map addone states
    where
        addone : Univ -> Univ
        addone (U pos score count) =
            let pos' = combine pos roll
                score' = score + cast pos'
                count' = mult * count
            in  U pos' score' count'


||| peel off winners
final : Integer -> Player -> Player
final otherCount pl = go pl.wins [] pl.states 
    where
        go : Integer -> List Univ -> List Univ -> Player
        go acc rval (u :: us) =
            if u.score >= 21 then go (acc + u.count*otherCount) rval us
            else go acc (u::rval) us
        go acc rval [] = P rval acc

||| open states for player
totalStates : Player -> Integer
totalStates p = foldl (+) 0 $ map (.count) p.states

rolls : List (Integer,Int)
rolls = [(1,3),(3,4),(6,5),(7,6),(6,7),(3,8),(1,9)]

step : Player -> Player
step p1 = { states := concatMap (doadd p1.states) rolls } p1

mkPlayer : Int -> Player
mkPlayer p = P [U p 0 1] 0

mkState : (Int,Int) -> State
mkState (pa,pb)= (mkPlayer pa, mkPlayer pb)

dlog : Show a => a -> a
dlog a = trace (show a) a

run2 : State -> (Integer,Integer)
run2 (p1,p2) =
    let p1' = final (totalStates p2) $ step p1
    in case p1' of
        P [] _ => (p1'.wins, p2.wins)
        _      => run2 $ dlog (p2,p1')
    -- case step st of
    --     (p1,p2) => (winsA, winsB)
    --     (p1,p2) => run2 $ trace (show st) st

main = do
    putStrLn "eg"
    run eg
    putStrLn "P2 \{show $ run2 $ mkState eg}"

    putStrLn "input"
    run input
    putStrLn "P2 \{show $ run2 $ mkState input}"