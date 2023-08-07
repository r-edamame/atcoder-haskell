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

module Main (main) where

import           Control.Applicative ((<|>))
import           Control.Lens ((.~), (%~), _1, _2, _3)
import           Control.Monad (replicateM, forM_, when, foldM, filterM)
import           Control.Monad.Cont.Class (MonadCont(callCC))
import           Control.Monad.Extra (whenM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Primitive (PrimMonad(PrimState), liftPrim)
import           Control.Monad.Reader (ReaderT(runReaderT))
import qualified Control.Monad.Reader.Class as Reader
import           Control.Monad.ST (ST, runST)
import           Control.Monad.State.Strict (StateT (StateT), evalStateT)
import qualified Control.Monad.State.Class as State
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont (ContT(ContT, runContT), evalContT, Cont, cont, runCont, evalCont)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import           Control.Monad.Writer.Class as Writer
import           Control.Monad.Writer.Strict (WriterT(runWriterT), Writer, runWriter, execWriter)
import           Data.Attoparsec.ByteString.Char8 (isDigit)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8)
import           Data.ByteString.Builder as BB
import           Data.ByteString.Builder.Internal as BB
import           Data.Char (intToDigit, digitToInt, isSpace)
import           Data.Functor.Identity (Identity(runIdentity))
import           Data.Foldable (foldrM, foldlM, toList, traverse_)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef (STRef, newSTRef, readSTRef, modifySTRef, writeSTRef)
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as VH
import qualified Data.Vector.Mutable as MV
import           Debug.Trace (trace)

----------------
---- solver ----
----------------

main :: IO ()
main = runInput $ do
    cs <- bs2v <$> readWord
    k <- readInt
    liftIO . print $ solv cs k

solv cs k = go 0 0 0 where
    dc = cumulative (+) 0 $ V.map f cs
    go l r ans
        | r == V.length cs = max ans (r-l)
        | (cs!r) == 'X' || dc!(r+1) - dc!l <= k = go l (r+1) ans
        | otherwise = go (l+1) r (max ans (r-l))
    f '.' = 1
    f _ = 0


-----------------
---- library ----
-----------------

debug :: Show a => String -> a -> a
debug label a = trace (concat @[] [label, ": ", show a]) a
-- debug label = id

-- type aliases
type Mat a = (V.Vector a, Int -> Int -> Int)

-- IO
---- Monad For Parse Input
type Input a = StateT ByteString (MaybeT IO) a
runInput :: Input () -> IO ()
runInput i = BS8.getContents >>= (fromJust<$>) . runMaybeT . evalStateT i
---- util
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
bs2v :: ByteString -> V.Vector Char
bs2v s = runST $ do
    v <- newGV
    BS8.foldl (\m c -> m >> pushGV v c) (return ()) s
    freezeGV v

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
insertHeap :: (PrimMonad m, Ord a) => MHeap (PrimState m) a -> a -> m ()
insertHeap (MHeap (heap, cmp)) a = do
    (v, l) <- do
        (v', l') <- liftPrim $ readSTRef heap
        if l' < MV.length v' then return (v', l') else do
            v'' <- MV.grow v' (MV.length v')
            liftPrim $ writeSTRef heap (v'', l')
            return (v'', l')
    VH.heapInsert cmp v 0 l a
    liftPrim $ modifySTRef heap (_2%~succ)
popHeap :: (PrimMonad m, Ord a) => MHeap (PrimState m) a -> m (Maybe a)
popHeap (MHeap (heap, cmp)) = do
    (v, l) <- liftPrim $ readSTRef heap
    if l == 0 then
        return Nothing
    else do
        a <- MV.read v 0
        liftPrim $ modifySTRef heap (_2%~pred)
        VH.pop cmp v 0 (l-1)
        return (Just a)
sizeHeap :: (PrimMonad m, Ord a) => MHeap (PrimState m) a -> m Int
sizeHeap (MHeap (heap, _)) = snd <$> liftPrim (readSTRef heap)
---- extensible vector
newtype GV s a = GV (STRef s (MV.MVector s a, Int, Int))
newGV :: ST s (GV s a)
newGV = MV.new 8 >>= \v -> GV <$> newSTRef (v, 8, 0)
pushGV :: GV s a -> a -> ST s ()
pushGV (GV s) a = do
    (v, cap, siz) <- readSTRef s
    if cap == siz then do
        v' <- MV.grow v (cap*7)
        MV.write v' siz a
        writeSTRef s (v', debug "cap" (MV.length v'), siz+1)
    else do
        MV.write v siz a
        writeSTRef s (v, cap, siz+1)
popGV :: GV s a -> ST s (Maybe a)
popGV (GV s) = do
    (v, cap, siz) <- readSTRef s
    if siz == 0 then
        return Nothing
    else do
        a <- MV.read v (siz-1)
        writeSTRef s (v, cap, siz-1)
        return (Just a)
freezeGV :: GV s a -> ST s (V.Vector a)
freezeGV (GV s) = do
    (v, _, siz) <- readSTRef s
    V.freeze . MV.take siz $ v
    ---- Mutable Vector Control
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
cumulative :: (b -> a -> b) -> b -> V.Vector a -> V.Vector b
cumulative f i = uncurry (flip V.snoc) . mapAccumL (\s a -> (f s a, s)) i
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
hasCircle :: Int -> V.Vector (Int, Int) -> Bool
hasCircle n es = runST $ evalContT $ do
    uf <- lift $ newUF n
    forM_ es $ \(s, t) -> do
        whenM (not <$> lift (uniteUF s t uf)) (quit True)
    return False
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
quit :: Monad m => r -> ContT r m a
quit = ContT . const . return
---- bits
digits :: Int -> V.Vector Int
digits d = V.iterateN d (`shiftL`1) 1
---- BFS
type BFS r q m a = ContT r (StateT (Seq q) m) a
runBfsM :: (Monad m, Eq q, Hashable q) => (q' -> BFS r q' m ()) -> r -> (q' -> q) -> Seq q' -> m r
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
runBfs :: (Eq q, Hashable q) => (q' -> BFS r q' Identity ()) -> r -> (q' -> q) -> Seq q' -> r
runBfs bfs r uq = runIdentity . runBfsM bfs r uq
runBfsM_ :: (Monad m, Eq q, Hashable q) => (q' -> BFS () q' m ()) -> (q' -> q) -> Seq q' -> m ()
runBfsM_ bfs = runBfsM bfs ()
queue :: Monad m => q -> BFS r q m ()
queue !q = State.modify (:|> q)
---- Heap DP
type HeapDP r v s m a = ContT r (ReaderT (MHeap s v) m) a
runHeapDP :: (PrimMonad m, Ord v, Hashable v, Foldable f) => HeapOrder -> (v -> HeapDP r v (PrimState m) m (f v)) -> [v] -> r -> m r
runHeapDP ho dp initial d = do
    heap <- newHeap ho
    traverse_ (insertHeap heap) initial
    runReaderT (evalContT (go heap HSet.empty)) heap
    where
        go heap used = do
            mp <- popHeap heap
            case mp of
                Nothing -> quit d
                Just p -> if HSet.member p used then go heap used else do
                    nexts <- dp p
                    forM_ nexts $ \n -> when (not $ HSet.member n used) (insertHeap heap n)
                    go heap (HSet.insert p used)

