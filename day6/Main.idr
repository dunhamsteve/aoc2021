module Main

import System.File
import Data.List
import Data.List1
import Data.String
import Data.Vect

Cast a b => Cast (List a) (List b) where cast = map cast

eg = "3,4,3,1,2"

splitChar : Char -> String -> List String
splitChar c s = filter (/="") $ toList $ split (== c) s

parse : String -> Maybe (List (Fin 9))
parse s = traverse foo (cast  (splitChar ',' s))
    where foo : Integer -> Maybe (Fin 9)
          foo n = integerToFin n 9

Fish = Vect 9 Int

total
mkFish : List (Fin 9) -> Fish
mkFish fish = go [0,0,0,0,0,0,0,0,0] fish
    where incr : Fish -> List Int -> Fish 
          go : Fish -> List (Fin 9) -> Fish
          go fish [] = fish
          go fish (x::xs) = go (updateAt x (+1) fish) xs  

total
iter : Fish -> Fish
iter fish = 
    let h = head fish
        fish' = snoc (tail fish) h
    in updateAt 6 (+h) fish'

total
run : Nat -> Fish -> Int
run 0 fish = sum fish
run (S n) fish = run n (iter fish)

main : IO ()
main = do
    Right text <- readFile "input.txt" | Left err => pure ()
    let Just fish = mkFish <$> parse text | Nothing => putStrLn "parse failed"
    putStrLn $ cast $ run 80 fish
    putStrLn $ cast $ run 256 fish


