import Debug.Trace
import Data.List

log : Show a => a -> IO ()
log = putStrLn . show

Point: Type
Point = (Int,Int)

Problem :Type
Problem = (Point,Point)

eg,input : Problem
eg = ((20,30),(-10,-5))
input = ((281,311),(-74,-54))

-- so x stops at vx(vx+1)/2 and minimum x is ceil((sqrt(8*x1)-1)/2)
-- y always hits 0 again at a speed of vy, and is vy+1 next, we want to maximize y
-- This happens in 2*(y+1) steps which is longer than it takes minx to get to the
-- the box in the examples
part1 : Problem -> Int
part1 ((x1,x2),(y1,y2)) =  let y = 1-y1 in y1 * (y1 + 1) `div` 2

-- And part2 we actually do need to scan... oh well

vy_of_t : Double -> Double -> Double
vy_of_t t y = (y/t) + (t-1)/2

yrange : (Int,Int) -> Int -> List Int
yrange (y1,y2) t = 
    let s = cast $ ceiling $ vy_of_t (cast t) (cast y1)
        e = cast $ floor   $ vy_of_t (cast t) (cast y2)
    in if s <= e then [s..e] else []

xrange : (Int,Int) -> Int -> List Int
xrange (x1,x2) t =
    let x1 = cast x1
        x2 = cast x2
        t = cast t
        -- past t1 at start and t2 for end, we need to switch to the forever values
        t1 = cast . ceiling $ (sqrt (1 + 8*x1) - 1) / 2
        t2 = cast . floor   $ (sqrt (1 + 8*x2) - 1) / 2
        s = if t >= cast t1 then t1 else cast $ ceiling $ (x1/t + (t-1)/2)
        e = if t >= cast t2 then t2 else cast $ floor $ (x2/t + (t-1)/2)
    in [s..e]

cross : List a -> List b -> List (a,b)
cross as bs = concatMap (\a => map (a,) bs) as

solve : Problem ->  (List (Int,Int))
solve ((x1,x2),(y1,y2)) =
    let tend = 2*(0-y1) in 
    concatMap (\t => cross (xrange (x1,x2) t) (yrange (y1,y2) t)) [1..tend]

nub2 : Ord a => List a -> List a
nub2 xs = go (sort xs)
    where go : List a -> List a
          go (a::b::xs) = if a == b then go (a::xs) else a :: go (b::xs)
          go xs = xs

run : Problem -> IO ()
run prob = do
    putStr "P1 "
    log $ part1 prob
    putStr "P2 "
    log $ length $ nub $ solve prob

main = do
    putStrLn "eg"
    run eg
    putStrLn "input"
    run input
