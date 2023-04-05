{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.State.Strict (StateT (StateT), evalStateT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector ((!))
import qualified Data.Vector as V

main :: IO ()
main = do
    input <- BS8.getContents
    let result = flip runInput input $ do
    putStrLn result



-- type aliases
type Mat a = V.Vector (V.Vector a)

-- IO
---- Monad For Parse Input
type Input a = StateT ByteString Maybe a
runInput :: Input a -> ByteString -> a
runInput i = fromJust . evalStateT i
---- util
dropSpaces = BS.dropWhile isSpaceWord8
---- Reader
readInt :: Input Int
readInt = StateT $ BS8.readInt . dropSpaces
readChar :: Input Char
readChar = StateT $ BS8.uncons . dropSpaces
readInts :: Int -> Input (V.Vector Int)
readInts n = V.replicateM n readInt
readIntMat :: Int -> Int -> Input (Mat Int)
readIntMat w h = V.replicateM h (readInts w)
readCharMat :: Int -> Int -> Input (Mat Char)
readCharMat w h = V.replicateM h (V.replicateM w readChar)
readTs :: Int -> Input (V.Vector (Int, Int))
readTs n = V.replicateM n ((,) <$> readInt <*> readInt)
readT3s :: Int -> Input (V.Vector (Int, Int, Int))
readT3s n = V.replicateM n ((,,) <$> readInt <*> readInt <*> readInt)
readT4s :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s n = V.replicateM n ((,,,) <$> readInt <*> readInt <*> readInt <*> readInt)
---- format
yesno :: Bool -> [Char]
yesno True = "Yes"
yesno False = "No"

-- Algorithms
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
