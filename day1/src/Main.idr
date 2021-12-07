module Main

import System.File
import Data.String

go : Int -> List Int -> Int -> Int
go prev (x :: xs) n = if  prev < x then go x xs (n + 1) else go x xs n
go _ [] n = n        


count : List Int -> Int
count [] = 0
count (x :: xs) = go x xs 0

readNumbers : String -> IO (List Int)
readNumbers fn = do
    Right only <- readFile fn | Left err => pure []
    pure $ map cast $ lines only



accum : List Int -> List Int
accum (a :: xs@(b :: c :: _)) = a + b + c :: accum xs
accum _ = []

window : List Int -> Int
window = count . accum

doit : String -> IO ()
doit fn = do
    nums <- readNumbers fn
    printLn $ "count " ++ fn ++ " " ++ (show $ count nums)
    -- printLn $ unwords (map show $ accum nums)
    printLn $ "window " ++ fn ++ " " ++ (show $ window nums)

main : IO ()
main = do 
    doit "eg.txt"
    doit "input.txt"

