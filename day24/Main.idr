
import Data.String.Parser
import System.File
import Data.Vect
import Data.List
import Data.String
import Debug.Trace

%hide Data.String.Parser.State

data Arg = R Char | I Int

Show Arg where
    show (R c) = singleton c
    show (I n) = show n

Eq Arg where
    R c == R d = c == d
    I n == I m = n == m
    _ == _ = False

Ord Arg where
    R c < R d = c < d
    I n < I m = n < m
    I _ < R _ = True
    _ < _ = False

data Node = St String Arg Arg

Show Node where
    show (St ins a b) = show (ins,a,b)

int : Parser Int
int = cast <$> integer

pArg : Parser Arg
pArg =   R <$> lexeme letter
     <|> I <$> int

pStmt : Parser Node
pStmt = do
    i <- lexeme $ takeUntil " "
    x <- pArg
    y <- case i of
            "inp" => pure (I 0)
            _ => pArg
    pure $ St i x y

pFile : Parser (List Node)
pFile = many (lexeme pStmt) <* eos

dump : List Node -> IO ()
dump env = sequence_ $ map printLn  env

record MState where
    constructor MS
    reg : (Vect 4 Int)
    key : List Int

Show MState where show (MS reg key) = "MS \{show reg} \{show key}"

Cast Char (Fin 4) where cast = restrict 3 . cast

get : MState -> Arg -> Int
get st (R c) = index (cast c) st.reg
get st (I n) = n

put : {auto st : MState} -> Arg -> Int -> MState
put (R c) n = { reg $= replaceAt (cast c) n } st
put _  _ = st

step : MState -> Node -> MState
step st (St inst a b) =
    case inst of
        "add" => put a $ get st a + get st b
        "mul" => put a $ get st a * get st b
        "mod" => put a $ get st a `mod` get st b
        "div" => put a $ get st a `div` get st b
        "eql" => put a $ if get st a == get st b then 1 else 0
        _ => trace "\{inst} not handled" st

initial : MState
initial = MS [0,0,0,0] []

Interpolation Nat where
    interpolate = show

||| choose biggest/smallest key with same state
winnow : List MState -> Bool -> List MState
winnow st big = go [] $ sortBy (comparing (.reg)) st
    where
        go : List MState -> List MState -> List MState
        go xs [] = xs
        go xs [x] = go (x::xs) []
        go xs (y::z::rest) =
            if y.reg /= z.reg then go (y::xs) (z::rest)
            else if y.key > z.key && big || y.key < z.key && not big then go (y::xs) rest
            else go (z::xs) rest

input : List MState -> List MState
input = concatMap tryAll
    where
        addInput : MState -> Int -> MState
        addInput st k = { reg $= replaceAt (cast 'w') k, key := (snoc st.key k)  } st
        tryAll st = map (addInput st) $ the (List Int) [1,2,3,4,5,6,7,8,9]

run : List Node -> Bool -> Maybe (List Int)
run instr big = go [initial] instr
    where
        go : List MState -> List Node -> Maybe (List Int)
        go sts [] =
            case map (.key) $ filter (\st => get st (R 'z') == 0) sts of
                x::xs => Just $ foldl (if big then max else min) x xs
                _ => Nothing

        go sts (i@(St "eql" a b) ::is) = 
            
            
            let after = map (\st => step st i) sts
                proj = map (\st => get st a) after
                (t,f) = partition (\st => 1 == get st a) after
            in if length t > 0 && length f > 0
                then go t is          -- if there is a split choose true.
                else go after is

        go sts (St "inp" _ _ ::is) = go (input $ winnow sts big) is
        go sts (i::is) = go (map (\st => step st i) sts) is

main : IO ()
main = do
    Right text <- readFile "input.txt" | Left err => printLn err
    let Right (inst,_) = parse pFile text | Left err => printLn err
    -- dump inst
    putStrLn "--- Part 1"
    let Just p1 = run inst True | Nothing => putStrLn "Failed"
    putStrLn $ concat $ map show p1

    putStrLn "--- Part 2"
    let Just p2 = run inst False | Nothing => putStrLn "Failed"
    putStrLn $ concat $ map show p2
