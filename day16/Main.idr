module Main

import System.File
import Data.List
import Data.String
import Data.String.Extra
import Control.Arrow
import Debug.Trace

log : Show a => a -> IO ()
log = putStrLn . show

dlog : Show a => String -> a -> a
dlog msg a = trace (msg ++ " " ++ show a) a

-- I could just use Data.String.Parser and translate the hex to strings of 1001, etc, but 
-- figured it'd be fun to write a parser.

record Parser (a : Type) where
    constructor P
    runParser : List Int -> Either String (a, List Int)

Functor Parser where
    map f p = P $ \s => case (p.runParser s) of
        Right (a, s') => Right (f a, s')
        Left err => Left err

Applicative Parser where
    pure x = P $ \s => Right (x,s)
    f <*> x = P $ \s => case (f.runParser s) of
        -- I think first would work here, but I can't get it to typecheck
        Right (f, s') => map (\(a,s'') => (f a, s'')) (x.runParser s')
        Left err => Left err

Monad Parser where
    ma >>= mab = P $ \s => case ma.runParser s of
        Right (a, s') => .runParser (mab a) s'
        Left err => Left err

Alternative Parser where
    empty = P $ \s => Left "No alt"
    a <|> b = P $ \s => case a.runParser s of
        Right x => Right x
        Left err => b.runParser s

h2b : Char -> List Int
h2b c = case c of
    '0' => [0,0,0,0]
    '1' => [0,0,0,1]
    '2' => [0,0,1,0]
    '3' => [0,0,1,1]
    '4' => [0,1,0,0]
    '5' => [0,1,0,1]
    '6' => [0,1,1,0]
    '7' => [0,1,1,1]
    '8' => [1,0,0,0]
    '9' => [1,0,0,1]
    'A' => [1,0,1,0]
    'B' => [1,0,1,1]
    'C' => [1,1,0,0]
    'D' => [1,1,0,1]
    'E' => [1,1,1,0]
    'F' => [1,1,1,1]
    _   => []

data Packet =
      Literal Int Int
    | Operator Int Int (List Packet)

-- have to do the dance with Nat to make it total
showPacket : Nat -> Packet -> String
showPacket Z _ = "..."
showPacket (S n) (Literal v l) = "Lit[" ++ show v ++ "," ++ show l ++ "]"
showPacket (S n) (Operator v op pkts) =
    let rec = join ", " (map (showPacket n) pkts) in "Op[" ++ show v ++ "," ++ show op ++ "," ++ rec ++ "]"

Show Packet where show = showPacket 10
    
many : Show a => Parser a -> Parser (List a)
many pa = ((::) <$> pa <*> many pa) <|> pure []

readLength : Nat -> Parser (List Packet)
readCount : Int -> Parser (List Packet)

read : Nat -> Parser Int
read n = P $ \s =>
    case s of
        [] => Left $ "EOF " ++ show n
        _  => let v = foldl (\a,n => a*2+n) 0 (take n s)
                   in Right (v, drop n s)

readLit : Int -> Parser Int
readLit a = read 5 >>= \n => if n >= 16 then readLit (a*16 + n - 16) else pure (a * 16 + n)

decode : Parser Packet
decode = do
    ver <- read 3
    tag <- read 3
    case tag of
        4 => Literal ver <$> readLit 0
        op => read 1 >>= \bit => case bit of
            0 => Operator ver op <$> (read 15 >>= readLength . cast)
            _ => Operator ver op <$> (read 11 >>= readCount  . cast)

readLength n = P $ \s =>
    let chunk = take n s
        rest  = drop n s
    in case .runParser (many decode) chunk of
        Right (xx,_) => Right (xx,rest)
        Right junk => Left $ "Extra junk in readLength" ++ show junk
        Left err => Left err

readCount 0 = pure []
readCount n = (::) <$> decode <*> readCount (n-1)

toBits : String -> List Int
toBits = concatMap h2b . unpack

vsum : Packet -> Int
vsum (Literal v _) = v
vsum (Operator v _ stuff) = foldl (\a,p => a + vsum p) v stuff

eval : Packet -> Either String Int
eval (Literal v val) = Right val
eval (Operator _ 0 rest) = sum <$> traverse eval rest
eval (Operator _ 1 rest) = product <$> traverse eval rest
eval (Operator _ 2 (p::ps)) = foldl min (eval p) $ map eval ps
eval (Operator _ 3 (p::ps)) = foldl max (eval p) $ map eval ps
eval (Operator _ 5 [a,b]) = pure $ if eval a > eval b then 1 else 0
eval (Operator _ 6 [a,b]) = pure $ if eval a < eval b then 1 else 0
eval (Operator _ 7 [a,b]) = pure $ if eval a == eval b then 1 else 0
eval pkt = Left $ "Bad Op " ++ show pkt

part1 : String -> IO ()
part1 text = case .runParser decode $ toBits text of
    Right (p,rest) => putStrLn $ "vsum=" ++ (show $ vsum p)
    Left err => putStrLn $ "Error: " ++ err

part2 : String -> IO ()
part2 text = case .runParser decode $ toBits text of
    Right (p,rest) => putStrLn $ "eval=" ++ (show $ eval p)
    Left err => putStrLn $ "Error: " ++ err

runFile : String -> IO ()
runFile fn = do
    putStrLn $ "** " ++ fn
    Right text <- readFile fn | Left err => log err
    part1 text    
    part2 text

