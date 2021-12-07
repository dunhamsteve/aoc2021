module Main

import System.File
import Data.String
import Data.List
import Data.List1
import Data.Vect
import Debug.Trace

Cast a b => Cast (List a) (List b) where
    cast xs = map cast xs

Board = Vect 5 (Vect 5 Int)

splitChar : Char -> String -> List String
splitChar c s = filter (/="") $ toList $ split (== c) s

intList : String -> List Int
intList x = cast $ splitChar ',' x

total
pline : String -> Maybe (Vect 5 Int)
pline s = toVect 5 $ cast $ splitChar ' ' s

total
toBoard : List String -> Maybe Board
toBoard xs = traverse pline xs >>= toVect 5

total
parse : String -> Maybe ?x
parse text = do
    let fst ::: boards' = split (=="") (lines text)
    boards <- traverse toBoard boards'
    header <- intList <$> head' fst
    pure (header,boards)


mark : Int -> Board -> Board
mark i board = map (map $ \n => if n == i then -1 else n) board

winner : Board -> Bool
winner board =
    let horiz = any (== -5) $ map sum board
        vert = any (== -5) $ map sum (transpose board)
    in horiz || vert

score : Board -> Int
score b = sum $ map (max 0) $ toList $ Data.Vect.concat b

total
run : List Int -> List Board -> List Int
run (n::ns) boards = 
    let boards' = map (mark n) boards
    in case find winner boards' of
        Just board => n * score board :: run (n::ns) 
            (assert_smaller boards $ filter (/= board) boards')
        Nothing => run ns boards'
run [] boards = []


main' : String -> IO ()
main' fn = do
    Right only <- readFile fn | Left err => pure ()
    let Just (header, boards) = parse only | Nothing => putStrLn "error"
    let value = run header boards
    putStrLn $ show value

main : IO ()
main = do
    main' "eg.txt"
    main' "input.txt"
    