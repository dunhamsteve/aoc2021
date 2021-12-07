module Main

import System.File
import Data.Bits
import Data.List
import Data.Fin
import Data.Nat
import Data.String
import Data.DPair
import Debug.Trace
import Data.String.Parser

-- need to parse, probably figure out what structure...

Values = List Int

data Line = LN Values Values
Show Line where
    show (LN a b) = show a ++ " | " ++ show b

total
digit : Char -> Int
digit 'a' = 1
digit 'b' = 2
digit 'c' = 4
digit 'd' = 8
digit 'e' = 16
digit 'f' = 32
digit 'g' = 64
digit _   = 0
-- digit c = bit $ restrict 63 (cast c - 97)


spaces' : Parser ()
spaces' =  many (char ' ') *> pure ()

parseValue : Parser Int
parseValue = do
    as <- some letter <* spaces'
    pure $ foldl (\a => \c => a .|. digit c) 0 as


parseLine : Parser Line
parseLine = LN <$> many parseValue <* char '|' <* spaces' <*> many parseValue

parseFile : Parser (List Line)
parseFile = some (parseLine <* spaces)

total
infer : Line -> Maybe Int
infer (LN a b) = do
    let all = a ++ b
    one <- find ((2==) . popCount) all
    four <- find ((4==) . popCount) all
    let value : Int -> Maybe Int
        value x = 
            let m1 = popCount $ x .&. complement one
                m4 = popCount $ x .&. complement four
            in case popCount x of
                    2 => Just 1
                    3 => Just 7
                    4 => Just 4
                    7 => Just 8
                    5 => if m1 == 3 then Just 3
                         else if m4 == 3 then Just 2
                         else if m4 == 2 then Just 5
                         else Nothing
                    6 => if m1 == 5 then Just 6
                         else if m4 == 3 then Just 0
                         else if m4 == 2 then Just 9
                         else Nothing
                    _ => Nothing 
    digits <- traverse value b
    pure $ foldl (\a => \x => a * 10 + x) 0 digits

main : IO ()
main = do
    Right text <- readFile "input.txt"
        | Left err => putStrLn $ show err
    let Right (val,_) = parse parseFile text
        | Left err => putStrLn err
    putStrLn $ show $ sum <$> traverse infer val


