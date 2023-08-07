{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM, forM_, when, foldM)
import           Control.Monad.Extra (whenM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader (ReaderT(runReaderT))
import qualified Control.Monad.Reader.Class as Reader
import           Control.Monad.ST (ST, runST)
import           Control.Monad.State.Strict (StateT (StateT), evalStateT)
import qualified Control.Monad.State.Class as State
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont (ContT(runContT), Cont, cont, runCont)
import qualified Control.Monad.Trans.Cont as Cont
import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import           Data.ByteString.Builder as BB
import           Data.ByteString.Builder.Internal as BB
import           Data.Char (intToDigit, digitToInt)
import           Data.Foldable (foldrM, foldlM)
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Debug.Trace (trace)

main :: IO ()
main = do
    result <- flip fmap BS8.getContents $ runInput $ do
        t <- readInt
        qs <- readTs t
        return $ solv <$> qs
    putStr . unlines . V.toList . V.map yesno $ result

solv :: (Int, Int) -> Bool
solv (a, s) = (s >= a) && V.all (\d -> (c .&. d) == 0) (digits 60) where
    c = (s - 2*a) .&. a

debug :: Show a => String -> a -> a
-- debug label a = trace (label ++ ": " ++ show a) a
debug label = id

-- type aliases
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
readInt1 :: Input Int
readInt1 = pred <$> readInt
readChar :: Input Char
readChar = StateT $ BS8.uncons . dropSpaces
readWord :: Input ByteString
readWord = StateT $ (\(a, b) -> if BS.length a == 0 then Nothing else Just (a, b)) . BS.break isSpaceWord8 . dropSpaces
readInts :: Int -> Input (V.Vector Int)
readInts n = V.replicateM n readInt
readInts1 :: Int -> Input (V.Vector Int)
readInts1 n = V.replicateM n readInt1
readIntMat :: Int -> Int -> Input (Mat Int)
readIntMat w h = (, \x y -> w*y + x) <$> V.replicateM (h*w) readInt
readIntMat1 :: Int -> Int -> Input (Mat Int)
readIntMat1 w h = (, \x y -> w*y + x) <$> V.replicateM (h*w) readInt1
readCharMat :: Int -> Int -> Input (Mat Char)
-- readCharMat w h = V.replicateM h (V.replicateM w readChar)
readCharMat w h = (, \x y -> w*y + x) <$> V.replicateM (h*w) readChar
readTs :: Int -> Input (V.Vector (Int, Int))
readTs n = V.replicateM n ((,) <$> readInt <*> readInt)
readTs1 :: Int -> Input (V.Vector (Int, Int))
readTs1 n = V.replicateM n ((,) <$> readInt1 <*> readInt1)
readT3s :: Int -> Input (V.Vector (Int, Int, Int))
readT3s n = V.replicateM n ((,,) <$> readInt <*> readInt <*> readInt)
readT3s1 :: Int -> Input (V.Vector (Int, Int, Int))
readT3s1 n = V.replicateM n ((,,) <$> readInt1 <*> readInt1 <*> readInt1)
readT4s :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s n = V.replicateM n ((,,,) <$> readInt <*> readInt <*> readInt <*> readInt)
readT4s1 :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s1 n = V.replicateM n ((,,,) <$> readInt1 <*> readInt1 <*> readInt1 <*> readInt1)
readBSs :: Int -> Input (V.Vector ByteString)
readBSs n = V.replicateM n readWord
---- format
yesno :: Bool -> [Char]
yesno True = "Yes"
yesno False = "No"
showCharMat :: Int -> Int -> V.Vector Char -> ByteString
showCharMat w h = BS.init . BS8.unlines . map (fst . BS8.unfoldrN w unconsV) . takes w
showIntMat :: Int -> Int -> V.Vector Int -> ByteString
showIntMat  w h = BS.init . BS8.unlines . map (BS8.unwords . V.foldr (\i r -> showBS i : r) []) . takes w
showBS :: Show a => a -> ByteString
showBS = BS8.pack . show
showT :: Show a => (a, a) -> ByteString
showT = BS8.pack . unwords . map show . t2l
t2l :: (a, a) -> [a]
t2l (a, b) = [a, b]
unconsV :: V.Vector a -> Maybe (a, V.Vector a)
unconsV v
    | V.length v == 0 = Nothing
    | otherwise = Just (V.head v, V.tail v)
takes :: Int -> V.Vector a -> [V.Vector a]
takes n v
    | V.length v == 0 = []
    | otherwise = let (s, e) = V.splitAt n v in s : takes n e

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
---- cumulative
cumulative :: V.Vector Int -> V.Vector Int
cumulative = uncurry (flip V.snoc) . mapAccumL (\s a -> (s + a, s)) 0
---- UnionFind
newtype UnionFind s = UnionFind (MV.MVector s Int, MV.MVector s Int, MV.MVector s Int)
newUF :: Int -> ST s (UnionFind s)
newUF n = do
    par <- V.thaw $ V.iterateN n (+1) 0
    rank <- V.thaw $ V.replicate n 0
    siz <- V.thaw $ V.replicate n 1
    return $ UnionFind (par, rank, siz)
rootUF :: Int -> UnionFind s -> ST s Int
rootUF x uf@(UnionFind (par, _, _))= do
    p <- MV.read par x
    if p == x
        then
            return x
        else do
            r <- rootUF p uf
            MV.write par x p
            return r
sameUF :: Int -> Int -> UnionFind s -> ST s Bool
sameUF x y uf = (==) <$> rootUF x uf <*> rootUF y uf
uniteUF :: Int -> Int -> UnionFind s -> ST s Bool
uniteUF x y uf@(UnionFind (par, rank, siz)) = do
    (rx, ry) <- (,) <$> rootUF x uf <*> rootUF y uf
    if rx == ry then
        return False
    else do
        cr <- (<) <$> MV.read rank rx <*> MV.read rank ry
        let (rx', ry') = if cr then (ry, rx) else (rx, ry)
        MV.write par ry' rx'
        whenM ((==) <$> MV.read rank rx <*> MV.read rank ry) $ MV.modify rank (+1) rx'
        MV.read siz ry >>= \c -> MV.modify siz (+c) rx
        return True
sizeUF :: Int -> UnionFind s -> ST s Int
sizeUF x uf@(UnionFind(_, _, siz)) = MV.read siz x
---- undirected graph
udg :: Int -> V.Vector (Int, Int) -> V.Vector (V.Vector Int)
udg n v = V.accumulate (flip V.cons) (V.replicate n V.empty) to where
    to :: V.Vector (Int, Int)
    to = v >>= \(x, y) -> [(x, y), (y, x)]
---- run length encoding
rle :: Eq a => V.Vector a -> V.Vector (a, Int)
rle = V.fromList . V.foldr step [] where
    step :: Eq a => a -> [(a, Int)] -> [(a, Int)]
    step a [] = [(a, 1)]
    step a l@((x, n):xs)
        | a == x = (a, n+1):xs
        | otherwise = (a, 1):l
---- exitable fold
foldlE :: Foldable f => (a -> b -> Either r a) -> a -> f b -> (a -> r) -> r
foldlE f d xs = runCont (foldlM (\a -> either quit return . f a) d xs)
foldrE :: Foldable f => (a -> b -> Either r b) -> b -> f a -> (b -> r) -> r
foldrE f d xs = runCont (foldrM (\a -> either quit return . f a) d xs)
quit :: r -> Cont r a
quit = cont . const
---- bits
digits :: Int -> V.Vector Int
digits d = V.iterateN d (`shiftL`1) 1

