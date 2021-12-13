module Main

import Data.String.Parser
import System.File
import Data.List
import Debug.Trace

log : Show a => a -> IO ()
log = putStrLn . show

trace' : Show a => a -> a
trace' a = trace (show a) a

-- By itself (Int,Int) has type (Type,Type)
Point = the Type (Int,Int)

pInt : Parser Int
pInt = cast <$> integer

pPoint : Parser Point
pPoint = (,) <$> pInt <* char ',' <*> pInt <* spaces

data Instr = FoldX Int | FoldY Int
Show Instr where
       show (FoldX x) = "foldx " ++ show x
       show (FoldY y) = "foldy " ++ show y

data Problem = P (List Point) (List Instr)
Show Problem where 
       show (P pts ins) = "Problem " ++ show pts ++ " " ++ show ins
       
pInstr : Parser Instr
pInstr = FoldX <$ string "fold along x=" <*> pInt <* spaces
       <|> FoldY <$ string "fold along y=" <*> pInt <* spaces

draw : List Point -> String
draw pts =
    let mx = foldl max 0 $ map fst pts
        my = foldl max 0 $ map snd pts
        drawLine : Int -> List Char
        drawLine y = map (\x => if (x,y) `elem` pts then '#' else ' ') [0..mx] 
    in pack $ concatMap (\y => '\n' ::drawLine y) [0..my]

pFile : Parser Problem
pFile = P <$> many pPoint <*> many pInstr

step : List Point -> Instr -> List Point
step pts (FoldX n) = sort $ nub $ map (\(x,y) => if x > n then (2*n-x,y) else (x,y)) pts
step pts (FoldY n) = sort $ nub $ map (\(x,y) => if y > n then (x,2*n-y) else (x,y)) pts

run : Problem -> List Point
run (P pts ins) = foldl (step) pts ins

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "** " ++ fn
    Right stuff <- readFile fn | Left err => log err
    let Right (prob,_) = parse pFile stuff | Left err => log err
    let P pts (first :: _)  = prob | _ => log "no instructions"
--     log prob
    log $ length $ step pts first
    let res = run prob
    putStrLn $ draw res

main = do
    runFile "eg.txt"
    runFile "input.txt"
