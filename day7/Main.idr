module Main

import System.File
import Data.List
import Data.List1
import Data.String
import Data.Vect

Cast a b => Cast (List a) (List b) where cast = map cast

splitChar : Char -> String -> List String
splitChar c s = filter (/="") $ toList $ split (== c) s

parse : String -> List Int
parse s = cast (splitChar ',' $ trim s)
    
foo : Int -> Int
foo n = let n' = abs n in n' * (n' + 1) `div` 2

run : List Int -> (Int -> Int) -> Int
run crabs f =
    let calc = \x => foldl (\a => \n => a + f (n - x)) 0 crabs
        mx = foldl max 0 crabs
        mn = foldl min 0 crabs 
        nums = map calc [mn .. mx]
    in case nums of 
        n :: ns => foldl min n ns
        [] => 0

puts : Show a => a -> IO ()
puts = putStrLn . show

main : IO ()
main = do
    Right text <- readFile "input.txt" | Left err => pure ()
    let crabs =  parse text
    -- puts crabs
    puts $ run crabs abs
    puts $ run crabs foo