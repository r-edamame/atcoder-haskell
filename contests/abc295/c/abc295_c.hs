{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.State.Strict (StateT (StateT), evalStateT, State, evalState)
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.State.Class as State
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Builder as BB
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Vector ((!))
import qualified Data.Vector as V
import Data.ByteString.Builder (intDec)

main :: IO ()
main = do
    input <- BS8.getContents
    let result = flip runInput input $ do
        n <- readInt
        as <- readInts n
        return $ solv as
    BS8.putStrLn result

-- (s -> a -> (s, b)) -> t a -> s -> t b

solv :: V.Vector Int -> ByteString
solv as = showBS $ V.sum @Int $ mapAccumL step Set.empty as where
    step :: Set Int -> Int -> (Set Int, Int)
    step s a
        | Set.member a s = (Set.delete a s, 1)
        | otherwise = (Set.insert a s, 0)
    go :: V.Vector Int -> Int -> State (Set Int) Int
    go v n
        | V.length v == 0 = return n
        | otherwise = do
            ok <- insert (V.head v)
            if ok then go (V.tail v) n else delete (V.head v) >> go (V.tail v) (n+1)


runWithSet :: State (Set a) a -> a
runWithSet = flip evalState Set.empty
insert :: Ord a => a -> State (Set a) Bool
insert a = do
    has <- State.gets (Set.member a)
    State.modify (Set.insert a)
    return (not has)
delete :: Ord a => a -> State (Set a) Bool
delete a = do
    has <- State.gets (Set.member a)
    State.modify (Set.delete a)
    return has

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
showBS :: Show a => a -> ByteString
showBS = BS8.pack . show

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
