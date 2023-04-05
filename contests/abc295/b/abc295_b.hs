{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM, forM_, when)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader (ReaderT(runReaderT))
import qualified Control.Monad.Reader.Class as Reader
import           Control.Monad.State.Strict (StateT (StateT), evalStateT)
import qualified Control.Monad.State.Class as State
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import           Data.ByteString.Builder as BB
import           Data.ByteString.Builder.Internal as BB
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Attoparsec.ByteString.Char8 (isDigit)
import Data.Char (intToDigit, digitToInt)
import Debug.Trace (trace)
import Control.Monad.Extra (whenM)

main :: IO ()
main = do
    input <- BS8.getContents
    let result = flip runInput input $ do
        h <- readInt
        w <- readInt
        mat <- readCharMat w h
        return $ solv w h mat
    BS8.putStrLn result

solv :: Int -> Int -> Mat Char -> ByteString
solv w h (mat, ix) =
    let
        blasts = [(ix (x+nx) (y+ny), '.') |
            x <- [0 .. w - 1],
            y <- [0 .. h - 1],
            isDigit (mat ! ix x y),
            let n = digitToInt $ mat ! ix x y,
            nx <- [- n .. n],
            0 <= x + nx && x + nx < w,
            ny <- [- n .. n],
            0 <= y + ny && y + ny < h,
            abs nx + abs ny <= n
            ]
        blasted = V.accum (const id) mat blasts
    in showCharMat w h blasted

-- type aliases
-- type Mat a = V.Vector (V.Vector a)
type Mat a = (V.Vector a, Int -> Int -> Int)

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
readIntMat w h = (, \x y -> w*y + x) <$> V.replicateM (h*w) readInt
readCharMat :: Int -> Int -> Input (Mat Char)
-- readCharMat w h = V.replicateM h (V.replicateM w readChar)
readCharMat w h = (, \x y -> w*y + x) <$> V.replicateM (h*w) readChar
readTs :: Int -> Input (V.Vector (Int, Int))
readTs n = V.replicateM n ((,) <$> readInt <*> readInt)
readT3s :: Int -> Input (V.Vector (Int, Int, Int))
readT3s n = V.replicateM n ((,,) <$> readInt <*> readInt <*> readInt)
readT4s :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s n = V.replicateM n ((,,,) <$> readInt <*> readInt <*> readInt <*> readInt)
---- format
yesno :: Bool -> ByteString
yesno True = "Yes"
yesno False = "No"
showCharMat :: Int -> Int -> V.Vector Char -> ByteString
showCharMat w h = BS.init . BS8.unlines . map (fst . BS8.unfoldrN w unconsV) . takes w
showIntMat :: Int -> Int -> V.Vector Int -> ByteString
showIntMat  w h = BS.init . BS8.unlines . map (BS8.unwords . V.foldr (\i r -> showBS i : r) []) . takes w

showBS :: Show a => a -> ByteString
showBS = BS8.pack . show

unconsV :: V.Vector a -> Maybe (a, V.Vector a)
unconsV v
    | V.length v == 0 = Nothing
    | otherwise = Just (V.head v, V.tail v)
takes :: Int -> V.Vector a -> [V.Vector a]
takes n v
    | V.length v == 0 = []
    | otherwise = let (s, e) = V.splitAt n v in s : takes n e

-- Algorithms
---- binery search
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
---- cumulative
cumulative :: V.Vector Int -> V.Vector Int
cumulative = uncurry (flip V.snoc) . mapAccumL (\s a -> (s + a, s)) 0
---- UnionFind
newtype UnionFind s = UnionFind (MV.MVector s Int, MV.MVector s Int, MV.MVector s Int)
type UFM s a =  ReaderT (UnionFind s) (ST s) a
withUF :: forall a. Int -> (forall s. UFM s a) -> a
withUF n m = runST $ do
    par <- V.thaw $ V.iterateN n (+1) 0
    rank <- V.thaw $ V.replicate n 0
    size <- V.thaw $ V.replicate n 1
    runReaderT m $ UnionFind (par, rank, size)
rootUF :: forall s. Int -> UFM s Int
rootUF x = do
    UnionFind (par, rank, siz) <- Reader.ask
    p <- lift $ MV.read par x
    if p == x
        then
            return x
        else do
            r <- rootUF p
            lift $ MV.write par x p
            return r
sameUF :: forall s. Int -> Int -> UFM s Bool
sameUF x y = (==) <$> rootUF x <*> rootUF y
uniteUF :: forall s. Int -> Int -> UFM s Bool
uniteUF x y = do
    UnionFind (par, rank, siz) <- Reader.ask
    rx <- rootUF x
    ry <- rootUF y
    if rx == ry then
        return False
    else lift $ do
        cr <- (<) <$> MV.read rank rx <*> MV.read rank ry
        let (rx', ry') = if cr then (ry, rx) else (rx, ry)
        MV.write par ry' rx'
        whenM ((==) <$> MV.read rank rx <*> MV.read rank ry) $ MV.modify rank (+1) rx'
        MV.read siz ry >>= \c -> MV.modify siz (+c) rx
        return True
sizeUF :: forall s. Int -> UFM s Int
sizeUF x = do
    UnionFind (_, _, siz) <- Reader.ask
    lift $ MV.read siz x


