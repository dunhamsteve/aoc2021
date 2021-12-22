import Data.String.Parser
import System.File
import Data.Buffer
import Data.List
import Debug.Trace

log : Show a => a -> IO ()
log = putStrLn . show

dlog : Show a => String -> a -> a
dlog msg a = trace (msg ++ " " ++ show a) a

Show a => Interpolation (a,a) where interpolate = show
Interpolation Bool where interpolate = show

record Range where
    constructor R
    x : (Int,Int)
    y : (Int,Int)
    z : (Int,Int)

Show Range where show s = "Range[\{s.x},\{s.y},\{s.z}]"

pInt : Parser Int
pInt = cast <$> integer

pSpan : Parser (Int,Int)
pSpan = (,) <$> pInt <* string ".." <*> pInt

pOp : Parser Bool
pOp = token "on" *> pure True <|> token "off" *> pure False

pLine : Parser (Bool,Range)
pLine = do
    op <- pOp
    x <- token "x=" *> pSpan <* token ","
    y <- token "y=" *> pSpan <* token ","
    z <- token "z=" *> pSpan <* spaces
    pure $ (op,R x y z)

pFile : Parser (List (Bool,Range))
pFile = many pLine <* eos

intSpan : (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
intSpan (a,b) (c,d)  =
    let x = max a c
        y = min b d
    in if x > y then Nothing else Just (x,y)

isectRange : Range -> Range -> Maybe Range
isectRange a b = [| R (intSpan a.x b.x) (intSpan a.y b.y) (intSpan a.z b.z) |]

initBox = R (-50,50) (-50,50) (-50,50)

filterInit : (Bool,Range) -> Maybe (Bool,Range)
filterInit (op,rect) = (op,) <$> isectRange initBox rect

filterOp : Range -> (Bool,Range) -> Maybe (Bool,Range)
filterOp a (op,b) = (not op,) <$> isectRange a b

step : (Bool,Range) -> List (Bool,Range) -> List (Bool,Range)
step (True,rect) prev = (True,rect) :: mapMaybe (filterOp rect) prev ++ prev
step (False,rect) prev = mapMaybe (filterOp rect) prev ++ prev

size : (Bool,Range) -> Integer
size (op,R x y z) =
    let len : (Int,Int) -> Integer
        len (x,y) = cast y - cast x + 1
    in  len x * len y * len z * (if op then 1 else -1)

process : List (Bool,Range) -> List (Bool,Range) -> Integer
process ((op,rect)::rest) out = process rest $ step (op,rect) out
process [] out = foldl (+) 0 $ map size out

runFile : String -> IO ()
runFile fn = do
    putStrLn "** \{fn}"
    Right text <- readFile fn | Left err => log err
    let Right (prob,_) = parse pFile text | Left msg => putStrLn "Parse failed \{msg}"
    putStr "P1 "
    log $ process (mapMaybe filterInit prob) []
    putStr "P2 "
    log $ process prob []

main : IO ()
main = do
    runFile "eg.txt"
    runFile "eg2.txt"
    runFile "eg3.txt"
    runFile "input.txt"
