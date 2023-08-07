{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}

module Main (main) where
import System.IO (hFlush, stdout)

main :: IO ()
main =  do
    n <- read @Int <$> getLine
    let mid = n`div`2
    i <- ask mid
    if i == 0 then go0 mid n else go1 1 mid
    where
        go0 l r
            | abs (l-r) <= 1 = answer l
            | otherwise = do
                let mid = (l+r)`div`2
                si <- ask mid
                if si == 0 then go0 mid r else go1 l mid
        go1 l r
            | abs (l-r) <= 1 = answer (r-1)
            | otherwise = do
                let mid = (l+r)`div`2
                si <- ask mid
                if si == 1 then go1 l mid else go0 mid r

answer i = putStrLn $ "! " ++ show i
ask i = putStrLn ("? " ++ show i) >> hFlush stdout >> read @Int <$> getLine
