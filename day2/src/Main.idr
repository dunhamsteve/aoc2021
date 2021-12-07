module Main

import System.File
import Data.String
-- import Data.Vect.Sort
-- import Data.SortedMap

data Command = Forward Int | Down Int | Up Int 

parseLine : String -> Maybe Command


parse : String -> IO (List (List String))
parse fn = do
    Right only <- readFile fn | Left err => pure []
    pure $ map words $ lines only


exec : (Int, Int) -> List String -> (Int, Int)
exec (a,b) [] = (a,b)
exec (a,b) ["forward", n] = (a + cast n, b)
exec (a,b) ["up", n]      = (a, b - cast n)
exec (a,b) ["down", n]    = (a, b + cast n)
exec _ _ = ?error


exec2 : (Int, Int, Int) -> List String -> (Int, Int, Int)
exec2 (p,d,a) ["forward", n] = (p + cast n, d + a * cast n, a)
exec2 (p,d,a) ["up", n]      = (p,d,a - cast n)
exec2 (p,d,a) ["down", n]    = (p,d,a + cast n)
exec2 _ _ = ?error2

doit : String -> IO ()
doit fn = do
    printLn $ "* " ++ fn
    cmds <- parse fn
    let (a,b) = foldl exec (0,0) cmds 
    printLn $ "p1 " ++ cast (a * b)
    let (p,d,a) = foldl exec2 (0,0,0) cmds
    -- printLn $ show (p,d,a)
    printLn $ "p2 " ++ cast (p * d)
    
    
main : IO ()
main = do 
    doit "eg.txt"
    doit "input.txt"

