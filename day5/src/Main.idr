module Main

import System.File
import Data.String
import Data.List
import Data.List1
import Data.Vect
import Data.String.Parser
import Data.Buffer

Cast a b => Cast (List a) (List b) where
    cast xs = map cast xs

record Point where
    constructor PT
    x : Int
    y : Int

record Line where
    constructor LN
    a : Point
    b : Point

-- data Point = PT Int Int
Show Point where
    show (PT x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- data Line = LN Point Point
Show Line where
    show (LN a b) = show a ++ " -> " ++ show b

pInt : Parser Int
pInt = cast <$> integer

pPoint : Parser Point
pPoint = PT <$> pInt <* char ',' <*> pInt <* spaces

pLine : Parser Line
pLine = LN <$> pPoint <* token "->" <*> pPoint 

pFile : Parser (List Line)
pFile = many pLine

delta : Int -> Int -> Int
delta a b = if a < b then 1
            else if a == b then 0
            else -1


process : List Line -> IO Int
process stuff = do
    let max_x = 1 + foldl max 0 (map (.a.x) stuff ++ map (.b.x) stuff)
    let max_y = 1 + foldl max 0 (map (.a.y) stuff ++ map (.b.y) stuff)
    
    Just mem <- newBuffer (max_x * max_y) | Nothing => pure 0
    let contrib : Int -> Int -> IO Int
        contrib a i = getByte mem i >>= \x => pure (if x > 1 then a + 1 else a)
    let addLine : Line -> IO ()
        addLine l = 
            let dx = delta l.a.x l.b.x
                dy = delta l.a.y l.b.y
                mark : Int -> Int -> IO ()
                mark x y = do
                    v <- getByte mem (x + y * max_x)
                    setByte mem (x + y * max_x) (v + 1)
                go : Int -> Int -> IO ()
                go x y = do
                    mark x y
                    if x /= l.b.x || y /= l.b.y
                        then go (x + dx) (y + dy)
                        else pure ()
            in go l.a.x l.a.y
    _ <- traverse addLine stuff
    result <- foldlM contrib 0 [0 .. max_x*max_y - 1]
    pure result

-- to do part 1, need to filter out the diagonal lines

main' : String -> IO ()
main' fn = do
    Right only <- readFile fn | Left err => putStrLn $ show err
    case parse pFile only of
        Right (result,_) => putStrLn =<< show <$> process result
        Left err => putStrLn $ "error " ++ err
    

main : IO ()
main = do
    main' "eg.txt"
    main' "input.txt"
    