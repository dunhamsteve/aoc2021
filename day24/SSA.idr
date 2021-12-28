-- This was my failed attempt at simplifying the code.
-- I'm checking it in just to record it.

-- I was going to add value range propagation next, to further simpify. After
-- seeing the mod 26's and the impossible comparison with 13 up front, I suspected
-- part 2 would expand the possible values to something like 1-25, which it did not.

import Data.String.Parser
import System.File
import Data.Vect
import Data.List
import Data.String
import Debug.Trace

%hide Data.String.Parser.State

-- Lots of stuff cancels out in the beginning at least

-- Lets parse it to some data, execute symbolically with some simplification.
-- It's only four pages of instructions, so we maybe can do the reduction at the end?


-- registers

data Arg = R Char Int | I Int

Show Arg where
    show (R c n) = singleton c ++ "_" ++ show n
    show (I n) = show n

Eq Arg where 
    R c i == R d j = c == d && i == j
    I n == I m = n == m
    _ == _ = False

Ord Arg where 
    R c i < R d j = (c,i) < (d,j)
    I n < I m = n < m
    I _ < R _ _  = True
    _ < _ = False

data Node = Val Arg | St String Arg Arg

Show Node where
    show (Val n) = show n
    show (St ins a b) = show (ins,a,b)

int : Parser Int
int = cast <$> integer

pArg : Parser Arg
pArg =   R <$> lexeme letter  <*> pure 0
     <|> I <$> int

-- pInst : Parser Inst
-- pInst =   Inp <$ token "inp"
--       <|> Add <$ token "add"
--       <|> Mul <$ token "mul"
--       <|> Div <$ token "div"
--       <|> Mod <$ token "mod"
--       <|> Eql <$ token "eql"

pStmt : Parser Node
pStmt = do
    i <- lexeme $ takeUntil " "
    x <- pArg
    y <- case i of
            "inp" => pure (I 0)
            _ => pArg
    pure $ St i x y
      
pFile : Parser (List Node)
pFile = many (lexeme pStmt) <* eos



Env = List (Arg,Node)

initEnv : Env
initEnv = [(R 'x' 0, Val (I 0)), (R 'y' 0, Val (I 0)), (R 'z' 0, Val (I 0)), (R 'w' 0, Val (I 0))]

latest : Env -> Arg -> Arg
latest _ (I n) = I n
latest env (R c i) = go env
    where
        go : Env -> Arg
        go ((R d i,_)::xs) = if c == d then R d i else go xs
        go ((I _,_)::xs) = go xs -- doesn't happen.
        go [] = ?fail $ trace (show env ++ show (R c i)) 1

||| Used to build environment from parsed data
addStmt : Env -> Node -> Env
addStmt env (St ins a b) =
    let a = latest env a
        b = latest env b
        R x n = a | _ => env
    in (R x (n+1), St ins a b) :: env
addStmt env _ = env        

isConstant : (Arg,Node) -> Bool
isConstant (_,(Val _)) = True
isConstant _ = False

subst : (Arg, Node) -> (Arg, Node)  -> (Arg, Node)
subst (v1, St ins a b) (v2, Val n) = (v1, St ins (go a) (go b))
    where go : Arg -> Arg
          go a = if a == v2 then n else a
subst x _ = x

constantProp : Env -> Env
constantProp env =
    let (constants,rest) = partition isConstant env
    in map (\x => foldl subst x constants) rest


getVars : Node -> List Arg
getVars (St _ a b) = [a, b]
getVars (Val a) = [a]


nub' : Ord a => List a -> List a
nub' = go . sort
    where go : List a -> List a
          go (x::y::xs) = if x == y then go (x::xs) else x :: go (y::xs)
          go x = x

dropUnused : Env -> Env
dropUnused [] = []
dropUnused env@(e :: rest) = e :: go rest
    where
        vars : List Arg
        vars = nub' $ concatMap (getVars . snd) env

        go : Env -> Env
        go (e@(v,_)::rest) = if v `elem` vars then e :: go rest else go rest
        go [] = []

simp : {auto env : Env} -> Node -> Node
simp (St "add" (I n) (I m)) = Val $ I $ n + m
simp (St "mul" _ (I 0)) = Val $ I 0
simp (St "mul" (I 0) _) = Val $ I 0
simp (St "div" a (I 1)) = Val a
simp (St "add" (I 0) x) = Val x
simp (St "add" x (I 0)) = Val x
simp (St "mod" (I 0) x) = Val $ I 0
simp (St "eql" r (I 0)) = case lookup r env of
    Just (St "eql" a b) => (St "neq" a b)
    _ => (St "eql" r (I 0))
simp x = x


simplify : Env -> Env
simplify env = map (mapSnd simp) env

run : Env -> Env
run env = 
    let env' =  dropUnused $ simplify $ trace "step\n" env
        (constants,rest) = partition isConstant env'
    in if null constants then rest
    else run $ map (\x => foldl subst x constants) rest

dump : Env -> IO ()
dump env = sequence_ $ map printLn $ reverse env

doFile : String -> IO ()
doFile fn = do
    putStrLn "** \{fn}"
    Right text <- readFile fn | Left err => printLn err
    let Right (inst,_) = parse pFile text | Left err => printLn err
    -- sequence_ $ map printLn inst
    let start = foldl addStmt initEnv inst
    dump start
    putStrLn "---"
    dump $ run start

    putStrLn "---"




main = doFile "input.txt"
