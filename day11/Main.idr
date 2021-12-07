module Main

import System.File
import Data.String
import Data.List
import Data.Vect
import Data.SortedMap
import Data.Buffer

log : Show a => a -> IO ()
log = putStrLn . show

enqueue : Int -> List Int -> (Int,Int) -> List Int
enqueue pt pts (dx,dy) =
    let x = dx + mod pt 10
        y = dy + div pt 10
    in if x >= 0 && x < 10 && y >=0 && y < 10
       then (x + y*10) :: pts
       else pts

adj : List (Int,Int)
adj = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

step : Buffer -> IO Int
step b = do
    -- putStr "Step " >> bufferData b >>= log
    -- increment value, if new value is 10, enqueue flash 
    let incr : List Int -> Int -> IO (List Int)
        incr queue pt = do
            v <- getByte b pt
            setByte b pt (v + 1)
            pure $ if v == 9 then foldl (enqueue pt) queue adj
                   else queue

    queue <- foldlM incr [] [0..99]
    -- putStr "incr: " >> log queue
    

    -- process queue (flashes) incrementing neighbors

    let process : List Int -> IO ()
        process [] = pure ()
        process (x :: xs) = process =<< incr xs x

    process queue

    -- zero out > 9 and accumulate count
    let zero : Int -> Int -> IO Int
        zero acc pos = do
            v <- getByte b pos
            if v > 9 then do setByte b pos 0; pure $ acc + 1
                 else pure $ acc
    foldlM zero 0 [0..99]
    
bigflash : Buffer -> Int -> IO Int
bigflash b n = step b >>= \v => if v == 100 then pure n else bigflash b (n + 1)


parse2 : String -> IO (Either String Buffer)
parse2 text = do
    Just mem <- newBuffer 100 | Nothing => pure $ Left "Alloc Failed"
    let addc : Int -> Char -> IO Int
        addc i '\n' = pure $ i
        addc i c    = do
            setByte mem i (cast c - 48) 
            pure $ i+1
    _ <- foldlM addc 0 (unpack text)
    pure $ Right mem

main' : String -> Int -> IO ()
main' fn cnt = do
    putStrLn fn
    Right text <- readFile fn
        | Left err => log err
    Right problem <-  parse2 text | Left msg => log msg 
    log =<< foldlM (\ a,_ => step problem >>= \n => pure $ a + n) 0 [1..cnt]
    
    Right problem <-  parse2 text | Left msg => log msg 
    putStr "big flash @ "
    log =<< bigflash problem 1
    
main : IO ()
main = do
    main' "eg.txt" 100
    main' "input.txt" 100