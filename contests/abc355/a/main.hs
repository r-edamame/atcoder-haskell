{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Lens ((.~), (%~), _1, _2, _3)
import           Control.Monad (replicateM, forM_, when, foldM, filterM)
import           Control.Monad.Cont.Class (MonadCont(callCC))
import           Control.Monad.Extra (whenM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Primitive (PrimMonad(PrimState), liftPrim)
import           Control.Monad.Reader (ReaderT(ReaderT, runReaderT))
import qualified Control.Monad.Reader.Class as Reader
import           Control.Monad.ST (ST, runST)
import           Control.Monad.State.Strict (StateT (StateT, runStateT), execStateT, evalStateT, State, runState, evalState, execState)
import qualified Control.Monad.State.Class as State
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont (ContT(ContT, runContT), evalContT, Cont, cont, runCont, evalCont)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import           Control.Monad.Writer.Class as Writer
import           Control.Monad.Writer.Strict (WriterT(runWriterT), execWriterT, Writer, runWriter, execWriter)
import qualified Data.Bifunctor as BF
import           Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import           Data.ByteString.Builder as BB
import           Data.ByteString.Builder.Internal as BB
import           Data.Char (intToDigit, digitToInt, isSpace, isDigit)
import           Data.Functor.Identity (Identity(runIdentity))
import           Data.Foldable (foldrM, foldlM, toList, traverse_, maximumBy, minimumBy)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import qualified Data.List as L
import qualified Data.List.Extra as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef (STRef, newSTRef, readSTRef, modifySTRef, writeSTRef)
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Tuple.Extra (both)
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as VH
import qualified Data.Vector.Mutable as MV
import           Debug.Trace (trace)
import           GHC.Records (HasField(..))
import           Text.Printf (printf)

----------------
---- solver ----
----------------

main :: IO ()
main = runInput $ do
    let al = Set.fromList @Int [1..3]
    as <- readInts 2
    let cs = al Set.\\ (Set.fromList . toList $ as)
    if Set.size cs /= 1 then
        liftIO . print $ -1
    else
        liftIO . print . Set.findMin $ cs

-- solv ::
solv = undefined

-----------------
---- library ----
-----------------

-- debugging
debugging :: Bool
#ifndef ATCODER
debugging = True
#else
debugging = False
#endif
debug :: Show a => String -> a -> a
debug label a
    | debugging = trace (concat @[] [label, ": ", show a]) a
    | otherwise = a
debugIntMat :: String -> Mat Int -> Mat Int
debugIntMat label mat
    | debugging = trace (concat @[] [label, "\n", BS8.unpack $ showIntMat mat]) mat
    | otherwise = mat

-- type aliases
type VI = V.Vector Int
type VC = V.Vector Char
type Mat a = V.Vector (V.Vector a)

-- IO
---- Monad For Parse Input
type Input a = StateT ByteString (MaybeT IO) a
runInput :: Input () -> IO ()
runInput i = BS8.getContents >>= (fromJust<$>) . runMaybeT . evalStateT i
---- util
dropSpaces :: ByteString -> ByteString
dropSpaces = BS.dropWhile isSpaceWord8
---- Reader
readInt :: Input Int
readInt = StateT $ MaybeT . pure . BS8.readInt . dropSpaces
readInt1 :: Input Int
readInt1 = pred <$> readInt
readChar :: Input Char
readChar = StateT $ MaybeT . pure . BS8.uncons . dropSpaces
readWord :: Input ByteString
readWord = StateT $ (\(a, b) -> if BS.length a == 0 then MaybeT (pure Nothing) else MaybeT (pure $ Just (a, b))) . BS.break isSpaceWord8 . dropSpaces
readWords :: Int -> Input (V.Vector ByteString)
readWords n = V.replicateM n readWord
readInts :: Int -> Input VI
readInts n = V.replicateM n readInt
readInts1 :: Int -> Input VI
readInts1 n = V.replicateM n readInt1
readIntMat :: Int -> Int -> Input (Mat Int)
readIntMat w h = V.replicateM h $ V.replicateM w readInt
readIntMat1 :: Int -> Int -> Input (Mat Int)
readIntMat1 w h = V.replicateM h $ V.replicateM w readInt1
readCharMat :: Int -> Int -> Input (Mat Char)
readCharMat w h = V.replicateM h $ V.replicateM w readChar
readT :: Input (Int, Int)
readT = (,) <$> readInt <*> readInt
readT1 :: Input (Int, Int)
readT1 = (,) <$> readInt1 <*> readInt1
readTs :: Int -> Input (V.Vector (Int, Int))
readTs n = V.replicateM n readT
readTs1 :: Int -> Input (V.Vector (Int, Int))
readTs1 n = V.replicateM n readT1
readT3s :: Int -> Input (V.Vector (Int, Int, Int))
readT3s n = V.replicateM n ((,,) <$> readInt <*> readInt <*> readInt)
readT3s1 :: Int -> Input (V.Vector (Int, Int, Int))
readT3s1 n = V.replicateM n ((,,) <$> readInt1 <*> readInt1 <*> readInt1)
readT4s :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s n = V.replicateM n ((,,,) <$> readInt <*> readInt <*> readInt <*> readInt)
readT4s1 :: Int -> Input (V.Vector (Int, Int, Int, Int))
readT4s1 n = V.replicateM n ((,,,) <$> readInt1 <*> readInt1 <*> readInt1 <*> readInt1)
toCharVector :: ByteString -> V.Vector Char
toCharVector = V.fromList . BS8.unpack
---- format
yesno :: Bool -> [Char]
yesno True = "Yes"
yesno False = "No"
showCharMat :: Mat Char -> ByteString
showCharMat = BS.init . BS8.unlines . map (BS8.unfoldr unconsV) . toList
showIntMat :: Mat Int -> ByteString
showIntMat = BS.init . BS8.unlines . map (BS8.unwords . V.foldr (\i r -> showBS i : r) []) . toList
showBS :: Show a => a -> ByteString
showBS = BS8.pack . show
showT :: Show a => (a, a) -> ByteString
showT = BS8.pack . unwords . map show . t2l
t2l :: (a, a) -> [a]
t2l (a, b) = [a, b]
printV ::  (a -> ByteString) -> V.Vector a -> Input ()
printV tobs v = liftIO $ do
    print v.len
    BS8.putStr . BS8.unlines . toList $ V.map tobs v
unconsV :: V.Vector a -> Maybe (a, V.Vector a)
unconsV v
    | V.length v == 0 = Nothing
    | otherwise = Just (V.head v, V.tail v)
takes :: Int -> V.Vector a -> [V.Vector a]
takes n v
    | V.length v == 0 = []
    | otherwise = let (s, e) = V.splitAt n v in s : takes n e

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf c a = if c a then Nothing else Just a
(!*) :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> m (Maybe a)
m !* i = traverse (MV.read m) $ nothingIf (outOf m) i
(!*=) :: PrimMonad m => MV.MVector (PrimState m) a -> (Int, a) -> m (Maybe ())
m !*= (i, a) = traverse (flip (MV.write m) a) $ nothingIf (outOf m) i
(!*%) :: PrimMonad m => MV.MVector (PrimState m) a -> (Int, a -> a) -> m (Maybe ())
m !*% (i, f) = traverse (MV.modify m f) $ nothingIf (outOf m) i
class Range r where
    outOf :: r -> Int -> Bool
instance Range (V.Vector a) where
    outOf v i = i<0 || V.length v<=i
instance Range (MV.MVector s a) where
    outOf v i = i<0 || MV.length v<=i

instance HasField "len" (V.Vector a) Int where
    getField = V.length
instance HasField "len" [a] Int where
    getField = length

instance HasField "len" (MV.MVector s a) Int where
    getField = MV.length
instance HasField "at" (V.Vector a) (Int -> a) where
    getField = (V.!)

instance HasField "at" (MV.MVector s a) (Int -> ST s a) where
    getField = MV.read

instance HasField "set" (MV.MVector s a) (Int -> a -> ST s ()) where
    getField = MV.write

instance HasField "modify" (MV.MVector s a) (Int -> (a -> a) -> ST s ()) where
    getField mv = flip $ MV.modify mv
instance HasField "swap" (MV.MVector s a) (Int -> Int -> ST s ()) where
    getField mv i j = do
        iv <- mv.at i
        jv <- mv.at j
        mv.set i jv
        mv.set j iv

instance HasField "find" (V.Vector a) ((a -> Bool) -> Maybe a) where
    getField v p = V.find p v
instance HasField "findIndex" (V.Vector a) ((a -> Bool) -> Maybe Int) where
    getField v p = V.findIndex p v

-- Data
---- Heap
data HeapOrder = HeapDesc | HeapAsc
newtype MHeap s a = MHeap (STRef s (MV.MVector s a, Int), VH.Comparison a)
newHeap :: (PrimMonad m, Ord a) => HeapOrder -> m (MHeap (PrimState m) a)
newHeap o = do
    let cmp = case o of
            HeapDesc -> compare
            HeapAsc -> flip compare
    v <- MV.new 8
    heap <- liftPrim $ newSTRef (v, 0)
    return $ MHeap (heap, cmp)
instance HasField "insert" (MHeap s a) (a -> ST s ()) where
    getField (MHeap (heap, cmp)) a = do
        (v, l) <- do
            (v', l') <- readSTRef heap
            if l' < v'.len then return (v', l') else do
                v'' <- MV.grow v' (MV.length v')
                writeSTRef heap (v'', l')
                return (v'', l')
        VH.heapInsert cmp v 0 l a
        modifySTRef heap (_2%~succ)
instance HasField "pop" (MHeap s a) (ST s (Maybe a)) where
    getField (MHeap (heap, cmp)) = do
        (v, l) <- readSTRef heap
        if l == 0 then
            return Nothing
        else do
            a <- v.at 0
            modifySTRef heap (_2%~pred)
            VH.pop cmp v 0 (l-1)
            return (Just a)
instance HasField "size" (MHeap s a) (ST s Int) where
    getField (MHeap (heap, _)) = snd <$> readSTRef heap
---- extensible vector
newtype GV s a = GV (STRef s (MV.MVector s a, Int, Int))
newGV :: ST s (GV s a)
newGV = MV.new 8 >>= \v -> GV <$> newSTRef (v, 8, 0)
instance HasField "push" (GV s a) (a -> ST s ()) where
    getField (GV s) a = do
        (v, cap, siz) <- readSTRef s
        if cap == siz then do
            v' <- MV.grow v (cap*7)
            v'.set siz a
            writeSTRef s (v', v'.len, siz+1)
        else do
            v.set siz a
            writeSTRef s (v, cap, siz+1)
instance HasField "pop" (GV s a) (ST s (Maybe a)) where
    getField (GV s) = do
        (v, cap, siz) <- readSTRef s
        if siz == 0 then
            return Nothing
        else do
            a <- v.at (siz-1)
            writeSTRef s (v, cap, siz-1)
            return (Just a)
instance HasField "freeze" (GV s a) (ST s (V.Vector a)) where
    getField (GV s) = do
        (v, _, siz) <- readSTRef s
        V.freeze . MV.take siz $ v
mapMV :: PrimMonad m => (a -> a) -> MV.MVector (PrimState m) a -> m ()
mapMV f v = go 0 where
    l = MV.length v
    go i
        | i == l = return ()
        | otherwise = MV.modify v f i >> go (i+1)
foldMV :: PrimMonad m => (b -> a -> b) -> b -> MV.MVector (PrimState m) a -> m b
foldMV f d v = go 0 d where
    l = MV.length v
    go i _d
        | i == l = return _d
        | otherwise = do
            a <- MV.read v i
            go (i+1) (f _d a)

-- Algorithms
lowerBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
lowerBound lower upper sat = go (lower-1) (upper+1) where
    go ok ng
        | abs (ok-ng) > 1 = let mid = (ok+ng) `div` 2 in if sat mid then go mid ng else go ok mid
        | otherwise = if ok == lower-1 then Nothing else Just ok
upperBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
upperBound lower upper sat = go (lower-1) (upper+1) where
    go ng ok
        | abs (ok-ng) > 1 = let mid = (ng+ok) `div` 2 in if sat mid then go ng mid else go mid ok
        | otherwise = if ok == upper+1 then Nothing else Just ok
---- cumulative
instance HasField "cumulative" (V.Vector a) ((a -> a -> a) -> a -> V.Vector a) where
    getField v f d = ($ v) $ uncurry (flip V.snoc) . mapAccumL (\s a -> (f s a, s)) d
cumulative2d :: (a -> a -> a) -> a -> Mat a -> Mat a
cumulative2d f d mat = uncurry (flip V.snoc) $ mapAccumL (\s a -> (V.zipWith f s a, s)) (V.replicate (V.length (hs!0)) d) hs where
    hs = V.map (\v -> v.cumulative f d) mat
---- UnionFind
newtype UnionFind s = UnionFind (MV.MVector s Int, MV.MVector s Int, MV.MVector s Int)
newUF :: Int -> ST s (UnionFind s)
newUF n = do
    par <- V.thaw $ V.iterateN n (+1) 0
    rank <- V.thaw $ V.replicate n 0
    siz <- V.thaw $ V.replicate n 1
    return $ UnionFind (par, rank, siz)
instance HasField "root" (UnionFind s) (Int -> ST s Int) where
    getField uf@(UnionFind (par, _, _)) x = do
        p <- MV.read par x
        if p == x
            then
                return x
            else do
                r <- uf.root p
                MV.write par x p
                return r
instance HasField "same" (UnionFind s) (Int -> Int -> ST s Bool) where
    getField uf x y = (==) <$> uf.root x <*> uf.root y
-- sameUF :: Int -> Int -> UnionFind s -> ST s Bool
-- sameUF x y uf = (==) <$> rootUF x uf <*> rootUF y uf
instance HasField "unite" (UnionFind s) (Int -> Int -> ST s Bool) where
    getField uf@(UnionFind (par, rank, _)) x y = do
        (rx, ry) <- (,) <$> uf.root x <*> uf.root y
        if rx == ry then
            return False
        else do
            cr <- (<) <$> MV.read rank rx <*> MV.read rank ry
            let (rx', ry') = if cr then (ry, rx) else (rx, ry)
            MV.write par ry' rx'
            whenM ((==) <$> MV.read rank rx <*> MV.read rank ry) $ MV.modify rank (+1) rx'
            return True
instance HasField "size" (UnionFind s) (Int -> ST s Int) where
    getField (UnionFind(_, _, siz)) = MV.read siz
---- undirected graph
udg :: Int -> V.Vector (Int, Int) -> Mat Int
udg n v = V.accumulate (flip V.cons) (V.replicate n V.empty) to where
    to :: V.Vector (Int, Int)
    to = v >>= \(x, y) -> [(x, y), (y, x)]
hasCircle :: Int -> V.Vector (Int, Int) -> Bool
hasCircle n es = runST $ evalContT $ do
    uf <- lift $ newUF n
    forM_ es $ \(s, t) -> do
        whenM (not <$> lift (uf.unite s t)) (quit True)
    return False
---- run length encoding

instance Eq a => HasField "rle" (V.Vector a) (V.Vector (a, Int)) where
    getField = V.fromList . V.foldr step [] where
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
quit :: Monad m => r -> ContT r m a
quit = ContT . const . return
---- bits
instance HasField "digits" Int VI where
    getField d = V.iterateN d (`shiftL`1) 1
---- BFS
type BFS r q m a = ContT r (StateT (Seq q) m) a
runBfsM :: (Monad m, Hashable q) => (q' -> BFS r q' m ()) -> r -> (q' -> q) -> Seq q' -> m r
runBfsM bfs ir uq = evalStateT (runContT (go HSet.empty) (const (return ir))) where
    -- go :: Monad m => HashSet q -> r -> BFS r q m r
    go used = do
        q <- State.get
        case q of
            Seq.Empty -> return ()
            a :<| q'
                | HSet.member (uq a) used -> State.put q' >> go used
                | otherwise -> do
                    State.put q'
                    bfs a
                    go (HSet.insert (uq a) used)
runBfs :: Hashable q => (q' -> BFS r q' Identity ()) -> r -> (q' -> q) -> Seq q' -> r
runBfs bfs r uq = runIdentity . runBfsM bfs r uq
runBfsM_ :: (Monad m, Hashable q) => (q' -> BFS () q' m ()) -> (q' -> q) -> Seq q' -> m ()
runBfsM_ bfs = runBfsM bfs ()
queue :: Monad m => q -> BFS r q m ()
queue !q = State.modify (:|> q)
---- Heap DP
runHeapDP :: (Monad m, Ord a) => (a -> ContT r m [a]) -> [a] -> r -> m r
runHeapDP f initial r = runContT (go (Set.fromList initial)) (const (return r)) where
    go set = do
        if Set.null set then return () else do
            let (a, set') = Set.deleteFindMin set
            nexts <- f a
            go (foldr Set.insert set' nexts)