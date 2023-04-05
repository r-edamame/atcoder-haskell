{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}

module Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Internal (c2w, isSpaceWord8)
import Data.Array.Unboxed as A
import Data.Array.ST (runSTArray, MArray (newArray))
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO(liftIO))

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> [a]
unfoldrN 0 _ _ = []
unfoldrN n f b = case f b of
    Nothing -> []
    Just (a, b') -> a : unfoldrN (n-1) f b'

-- IO
dropSpaces = BS.dropWhile isSpaceWord8
readInt :: ByteString -> Maybe (Int, ByteString)
readInt = BS8.readInt . dropSpaces
readInts :: Int -> ByteString -> Vector Int
readInts n = V.unfoldrN n readInt
readIntMat :: Int -> Int -> ByteString -> A.Array (Int, Int) Int
readIntMat w h = A.listArray ((0, 0), (h-1, w-1)) . unfoldrN (w*h) readInt
readCharMat :: Int -> Int -> ByteString -> A.Array (Int, Int) Char
readCharMat w h = A.listArray ((0,0), (h-1, w-1)) . unfoldrN (w*h) (BS8.uncons . dropSpaces)
getNLines :: Int -> IO ByteString
getNLines n = do
    ls <- replicateM n BS8.getLine
    return $ BS.concat ls
yesno :: Bool -> [Char]
yesno True = "Yes"
yesno False = "No"

lowerBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
lowerBound lower upper sat = go (lower-1) (upper+1) where
    go ok ng
        | abs (ok-ng) > 1 = let mid = ok+ng `div` 2 in if sat mid then go mid ng else go ok mid
        | otherwise = if ok == lower-1 then Nothing else Just ok
upperBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
upperBound lower upper sat = go (lower-1) (upper+1) where
    go ng ok
        | abs (ok-ng) > 1 = let mid = ng+ok `div` 2 in if sat mid then go ng mid else go mid ok
        | otherwise = if ok == upper+1 then Nothing else Just ok


main :: IO ()
main = do
    print 0

