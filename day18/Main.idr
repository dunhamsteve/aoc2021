
import System.File
import Data.String

log : Show a => a -> IO ()
log = putStrLn . show

-- Initially I parsed into a tree, but the mutation works better
-- with a token list.

data Tok = LB | RB | N Int

Show Tok where 
    show LB = "["
    show RB = "]"
    show (N n) = show n

SNum = List Tok

total
parse : List Char -> SNum
parse ('['::cs) = LB :: parse cs
parse (']'::cs) = RB :: parse cs
parse (','::cs) = parse cs
parse (c :: cs) = N (cast c - 48) :: parse cs
parse [] = []

total
explode : SNum -> Either SNum SNum
explode ll = go 0 [] ll
    where add : Int -> SNum -> SNum
          add n (N m::rest) = N (n+m) :: rest
          add n (t::ts) = t::(add n ts)
          add _ [] = []
          go : Int -> SNum -> SNum -> Either SNum SNum
          go 4 prev (LB::N a::N b::RB::rest) = Right $ (reverse $ add a prev) ++ N 0 :: add b rest
          go d prev (LB::rest) = go (d+1) (LB :: prev) rest
          go d prev (RB::rest) = go (d-1) (RB :: prev) rest
          go d prev (t ::rest) = go d (t::prev) rest
          go 0 prev [] = Left $ reverse prev
          go a b c = Left [] -- unbalanced brackets

total
splitNum : SNum -> Either SNum SNum
splitNum ts = go [] ts
    where go : SNum -> SNum -> Either SNum SNum
          go prev (N n::ts) = 
              if n < 10 then go (N n::prev) ts
              else let a = n `div` 2
                       b = n - a
                   in Right $ reverse prev ++ LB :: N a :: N b :: RB :: ts
          go prev (t::ts) = go (t::prev) ts
          go prev [] = Left $ reverse prev

reduce : SNum -> SNum
reduce sn = case explode sn of
    Right sn => reduce sn
    Left sn => case splitNum sn of
        Right sn => reduce sn
        Left sn  => sn

add : SNum -> SNum -> SNum
add a b = reduce $ LB :: a ++ b ++ [RB]

-- We use a stack to do this since we don't have a parse tree
total
magnitude : SNum -> Int
magnitude ts = go [] ts where
    go : List Int -> SNum -> Int
    go stk (LB::rest) = go stk rest
    go stk (N n::rest) = go (n::stk) rest
    go (a::b::stk) (RB::rest) = go (a*2+b*3::stk) rest
    go [a] [] = a
    go _ _ = 0 -- unbalanced brackets

run : String -> IO ()
run fn = do
    putStrLn $ "* " ++ fn
    Right text <- readFile fn | Left err => log err
    let nums = map (parse . unpack) $ lines text
    let t::ts = nums | [] => log "empty file"
    putStr "P1 "
    log $ magnitude $ foldl add t ts
    putStr "P2 "
    log $ foldl max 0 $ concatMap (\x => map (magnitude . add x) nums) nums

main : IO ()
main = do
    run "eg.txt"
    run "input.txt"