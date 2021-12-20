
import System.File
import Data.List
import Data.List1
import Data.String
import Data.SortedMap
import Debug.Trace

||| The basic approach here is to look at a pile of the differences between points, which are normalized to:
|||
|||    For a point (x,y,z), only x can be negative, and |x|<= y <= z 
|||
||| Then we try to find matching differences between two sensors, trace back the transforms that were needed to
||| normalize, and check if this mapping yields us 12 matches.

log : Show a => a -> IO ()
log a = putStrLn $ show a

||| This requires Ord, but is much faster than the standard library nub
||| Not sure if it matters in this problem
nub' : Ord a => List a -> List a
nub' = go . sort
    where go : List a -> List a
          go (x::y::xs) = if x == y then go (x::xs) else x :: go (y::xs)
          go x = x

Point : Type
Point = (Int,Int,Int)

Cast a c => Cast b d => Cast (a,b) (c,d) where
    cast (a,b) = (cast a,cast b)

ptuple : String -> Maybe Point
ptuple s = case forget $ split (==',') s of
    [a,b,c] => Just $ cast (a,b,c)
    _       => Nothing

pchunk : List String -> List Point
pchunk = catMaybes . map ptuple

||| A rotation transform
data Trans = RotX | RotY | RotZ

Show Trans where
    show RotX = "RX"
    show RotY = "RY"
    show RotZ = "RZ"

Eq Trans where
    RotX == RotX = True
    RotY == RotY = True
    RotZ == RotZ = True
    _ == _ = False

||| MatchInfo  holds original points, normalization transformation, source for
||| a normalized point
record MatchInfo  where
    constructor F
    vec : Point
    start : Point
    end : Point

    trans : List Trans

Show MatchInfo  where
    show (F v s e ts) =
        "F " ++ unwords [show v, show s, show e, show ts]

sub : Point -> Point -> Point
sub (a,b,c) (d,e,f) = (a-d, b-e, c-f)

mkMatch : Point -> Point -> MatchInfo
mkMatch x y = F (sub x y) x y []

app : Trans -> Point -> Point
app op (x,y,z) = case op of
    RotX => (x,-z,y)
    RotY => (z,y,-x)
    RotZ => (-y,x,z)

iapp : Trans -> Point -> Point
iapp op (x,y,z) = case op of
    RotX => (x,z,-y)
    RotY => (-z,y,x)
    RotZ => (y,-x,z)

addTrans : Trans ->MatchInfo  -> MatchInfo
addTrans t (F (x,y,z) a b ts) = case t of
    RotX => F (x,-z,y) a b (t::ts)
    RotY => F (z,y,-x) a b (t::ts)
    RotZ => F (-y,x,z) a b (t::ts)

||| return a point and the transformation that normalized it
normalize : MatchInfo  -> MatchInfo
normalize pt@(F (x,y,z) _ _ _) =
    if abs y > abs z then normalize $ addTrans RotX pt
    else if abs x > abs z then normalize $ addTrans RotY pt
    else if abs x > abs y then normalize $ addTrans RotZ pt
    else if z < 0 then normalize $ addTrans RotY $ addTrans RotY pt
    else if y < 0 then normalize $ addTrans RotZ $ addTrans RotZ pt
    else pt

||| Takes a list of points and returns a list of diffs of unordered vectors of points
vectors : List Point -> List MatchInfo
vectors (x :: xs) = map (normalize . mkMatch x) xs ++ vectors xs
vectors [] = []

mkTable : List MatchInfo  -> SortedMap Point MatchInfo
mkTable = fromList . map (\f => (f.vec,f))

||| Describes a transform
record Transform where
    constructor TR
    shift : Point
    forward : List Trans
    reverse : List Trans

Show Transform where show (TR a b c) = show ("TR",a,b,c)

add : Point -> Point -> Point
add (a,b,c) (d,e,f) = (a+d,b+e,c+f)

-- apply a transform
aptrans : Transform -> Point -> Point
aptrans t pt = add t.shift $ foldl (flip iapp) (foldr app pt t.forward) t.reverse

mktrans : MatchInfo -> MatchInfo -> Transform
mktrans a b =
    let shift = sub a.start $ aptrans (TR (0,0,0) b.trans a.trans) b.start
    in TR shift b.trans a.trans

||| Sensor holds the bits that we want to hand around with a sensor, the actual points and a lookup table.
record Sensor where
    constructor S
    points : List Point
    table : SortedMap Point MatchInfo

mkSensor : List Point -> Sensor
mkSensor pts = S pts (mkTable . vectors $ pts)

match : Sensor -> Sensor -> Maybe Transform
match s1 s2 = go s1.table (values s2.table)
    where
        go : SortedMap Point MatchInfo  -> List MatchInfo  -> Maybe Transform
        go tab [] = Nothing
        go tab (f@(F vec _ _ _) :: xs) =
            case lookup vec tab of
                Just f2 =>
                    let tr = mktrans f2 f
                        matches = intersect s1.points $ map (aptrans tr) s2.points
                    in if length matches >= 12
                        then Just tr
                        else go tab xs
                _ => go tab xs

run : (Sensor,Transform) -> List Point
run (s,tr) = map (aptrans tr) s.points

-- match sensors to first one, collecting points
-- a successfull match adds the sensor.
findMatches : List1 Sensor -> (List Point, List Sensor, List Point)
findMatches (s:::rest) = go [] [] rest
    where
        -- for the foldl below, this runs findMatches with a given sensor/transform,
        -- which returns 'base' space, and the applies 'tr' to map points back to parent
        recur : (List Point, List Sensor, List Point) -> (Sensor,Transform) -> (List Point, List Sensor, List Point)
        recur (pts,todo,locs) (base,tr) =
            let (pts2,rest,locs') = findMatches (base:::todo) in (pts ++ map (aptrans tr) pts2, rest, locs ++ map (aptrans tr) locs')

        go : List (Sensor,Transform) -> List Sensor -> List Sensor -> (List Point, List Sensor, List Point)
        go hits miss (t::ts) = case match s t of
            Just trans => go ((t,trans)::hits) miss ts
            Nothing    => go hits (t::miss) ts
        go hits miss [] = foldl recur (s.points ++ concatMap run hits, miss, map (.shift . snd) hits) hits

manhattan : Point -> Point -> Int
manhattan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)

maxhattan : List Point -> Int
maxhattan (x::xs) = foldl max (maxhattan xs) (map (manhattan x) xs)
maxhattan [] = 0

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "* " ++ fn
    Right text <- readFile fn | Left err => log err
    let chunks = map (catMaybes . map ptuple) (splitOn "" (lines text))
    let sensors = map mkSensor chunks
    let (pts, missing, locs) = findMatches sensors
    if length missing == 0
        then putStrLn $ "P1 = " ++ (show $ length $ nub' pts)
        else putStrLn $  "Miss " ++ (show $ length missing)
    putStrLn $ "P2 = " ++ (show $ maxhattan $ (0,0,0) :: locs)

main : IO ()
main = do
    runFile "eg.txt"
    runFile "input.txt"
