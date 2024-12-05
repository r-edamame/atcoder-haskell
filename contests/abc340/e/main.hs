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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

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
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
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
import           Data.Ix (Ix(..))
import qualified Data.List as L
import qualified Data.List.Extra as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid (Sum(..))
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
import           Data.Word (Word64)
import           Debug.Trace (trace)
import           GHC.Records (HasField(..))
import           Text.Printf (printf)

----------------
---- solver ----
----------------

main :: IO ()
main = runInput $ do
    n <- readInt
    m <- readInt
    as <- readInts n
    bs <- readInts m
    liftIO . putStrLn . unwords . map show $ solv n m as bs

solv :: Int -> Int -> VI -> VI -> [Int]
solv n m as bs = map getSum . take n . dualSegToList $ foldl step ini bs where
    ini = dualSegMk (toList (V.map Sum as))
    l2p = least2pow n
    step seg b = let
        bv = getSum $ dualSegGet b seg
        (r, m) = divMod bv n
        in seg
            |> dualSegMod (+Sum (-bv)) b (b+1)
            |> dualSegMod (+Sum (bv`div`n)) 0 l2p
            |> dualSegMod (+Sum 1) (b+1) (min n (b+1+m))
            |> dualSegMod (+Sum 1) 0 (max 0 (b+1+m-n))

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
yesno :: Bool -> Input ()
yesno = liftIO . putStrLn . (!!) ["No", "Yes"] . fromEnum
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

(|>) :: a -> (a -> b) -> b
a |> f = f a
if_ :: Bool -> a -> a -> a
if_ True a _= a
if_ False _ b = b
if' :: Bool -> (a -> a) -> (a -> a)
if' b f = if_ b f id
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

instance HasField "len" ByteString Int where
    getField = BS8.length
instance HasField "substr" ByteString (Int -> Int -> ByteString) where
    getField bs i j = BS8.drop i (BS8.take j bs)

instance HasField "slice" (V.Vector a) (Int -> Int -> V.Vector a) where
    getField v l r = V.slice l (l-r) v
instance HasField "swap" (V.Vector a) ((Int, Int) -> (Int, Int) -> V.Vector a) where
    getField v s1 s2 = V.concat [v.slice 0 l0, v.slice l1 r1, v.slice r0 l1, v.slice l0 r0, v.slice r1 v.len] where
        (l0, r0) = min s1 s2
        (l1, r1) = max s1 s2

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
---- Heap DP
runHeapDP :: (Monad m, Ord a) => (a -> ContT r m [a]) -> [a] -> r -> m r
runHeapDP f initial r = runContT (go (Set.fromList initial)) (const (return r)) where
    go set = do
        if Set.null set then return () else do
            let (a, set') = Set.deleteFindMin set
            nexts <- f a
            go (foldr Set.insert set' nexts)

-- search
class PushPop pp where
    type Elem pp
    push :: Elem pp -> pp -> pp
    pop :: pp -> Maybe (Elem pp, pp)
searchM :: (PushPop pp, Hashable a, Monad m, Foldable f) => (Elem pp -> m (f (Elem pp))) -> (Elem pp -> a) -> pp -> m (HashSet a)
searchM f uq = go HSet.empty where
    p dup a pp = if HSet.member a dup then pp else push a pp
    go dup pp = case pop pp of
        Just (a, pp')
            | HSet.member (uq a) dup -> go dup pp'
            | otherwise -> f a >>= go (HSet.insert (uq a) dup) . foldr push pp'
        Nothing -> return dup
search :: (PushPop pp, Hashable a, Foldable f) => (Elem pp -> f (Elem pp)) -> (Elem pp -> a) -> pp -> HashSet a
search f uq = runIdentity . searchM (return . f) uq
searchFindM :: (Monad m, PushPop pp, Hashable a, Foldable f) => (Elem pp -> m (Either b (f (Elem pp)))) -> (Elem pp -> a) -> pp -> m (Maybe b)
searchFindM f uq ini = flip runContT (return . const Nothing) $ searchM f' uq ini where
    f' a = lift (f a) >>= \case
        Right nx -> return nx
        Left res -> ContT $ \_ -> return (Just res)
searchFind :: (PushPop pp, Hashable a, Foldable f) => (Elem pp -> Either b (f (Elem pp))) -> (Elem pp -> a) -> pp -> Maybe b
searchFind f uq ini = runIdentity $ searchFindM (return . f) uq ini

instance PushPop (Seq a) where
    type Elem (Seq a) = a
    push a sq = sq Seq.|> a
    pop = \case
        Seq.Empty -> Nothing
        a :<| sq -> Just (a, sq)
instance PushPop [a] where
    type Elem [a] = a
    push a st = a : st
    pop = \case
        [] -> Nothing
        (x:xs) -> Just (x, xs)
instance Ord a => PushPop (Set a) where
    type Elem (Set a) = a
    push = Set.insert
    pop st
        | Set.null st = Nothing
        | otherwise = Just $ Set.deleteFindMax st

-- DP
class Semiring r where
    srzero :: r
    srone :: r
    srplus :: r -> r -> r
    srmul :: r -> r -> r

newtype MaxPlus = MaxPlus Int deriving (Eq, Ord)
instance Semiring MaxPlus where
    srzero = MaxPlus minBound
    srone = MaxPlus 0
    srplus (MaxPlus x) (MaxPlus y) = MaxPlus (max x y)
    srmul (MaxPlus x) (MaxPlus y) = MaxPlus (x + y)

instance Semiring Bool where
    srzero = False
    srone = True
    srplus = (||)
    srmul = (&&)

dp :: forall r i. (Semiring r, Ix i, Eq r) => [i] -> (i, i) -> (i -> Maybe r) -> (i -> [(r, i)]) -> r
dp target rng isTrivial subproblems = runST $ do
    table <- MA.newArray rng srzero
    foldl srplus srzero <$> traverse (go table) target
    where
        go :: forall s. STA.STArray s i r -> i -> ST s r
        go table i
            | Just a <- isTrivial i = return a
            | otherwise = do
                v <- MA.readArray table i
                if v /= srzero then
                    return v
                else do
                    a <- foldl srplus srzero <$> traverse (\(s, i') -> srmul s <$> go table i') (subproblems i)
                    MA.writeArray table i a
                    return a

-- mint
mintMod :: Word64
mintMod = 998244353
mint :: Word64 -> Mint
mint = Mint . (`mod` mintMod)
newtype Mint = Mint Word64 deriving (Eq, Ord)
instance Num Mint where
    (Mint m) + (Mint n) = Mint $ (m + n) `mod` mintMod
    (Mint m) * (Mint n) = Mint $ (m * n) `mod` mintMod
    signum _ = 1
    abs m = m
    fromInteger = mint . fromIntegral
    negate (Mint m) = Mint (mintMod - m)
instance Show Mint where
    show (Mint m) = show m

-- Segment Tree
data Seg a where
    SegNode :: Int -> a -> (Seg a) -> (Seg a) -> Seg a
    SegLeaf :: a -> Seg a
least2pow :: Int -> Int
least2pow n = head . filter (n<=) $ iterate (*2) 2
instance HasField "len" (Seg a) Int where
    getField (SegLeaf _) = 1
    getField (SegNode l _ _ _) = l
instance HasField "value" (Seg a) a where
    getField (SegLeaf a) = a
    getField (SegNode _ a _ _) = a
segMk :: Monoid m => [m] -> Seg m
segMk xs = mk (map SegLeaf (xs ++ replicate (least2pow l - l) mempty)) where
    l = length xs
    mk [s] = s
    mk ss = mk (gp ss)
    gp [] = []
    gp (a:b:s) = SegNode (a.len * 2) (a.value <> b.value) a b : gp s
    gp _ = undefined
segDump :: (a -> String) -> Seg a -> [String]
segDump sh (SegLeaf a) = [sh a]
segDump sh (SegNode _ a cl cr) = sh a : zipWith (\a b -> a ++ " " ++ b) (segDump sh cl) (segDump sh cr)
segPut :: Monoid a => Seg a -> Int -> a -> Seg a
segPut seg i a = segMod seg i (const a)
segMod :: Monoid a => Seg a -> Int -> (a -> a) -> Seg a
segMod (SegLeaf a) _ f = SegLeaf (f a)
segMod (SegNode ln _ cl cr) i f
    | i .&. (ln`shiftR`1) == 0 = let cl' = segMod cl i f in SegNode ln (cl'.value <> cr.value) cl' cr
    | otherwise = let cr' = segMod cr i f in SegNode ln (cl.value <> cr'.value) cl cr'
segGet :: Seg a -> Int -> a
segGet (SegLeaf a) _ = a
segGet (SegNode ln _ cl cr) i
    | i .&. (ln`shiftR`1) == 0 = segGet cl i
    | otherwise = segGet cr i
segSum :: Monoid a => Seg a -> Int -> Int -> a
segSum (SegLeaf a) _ _ = a
segSum (SegNode ln a cl cr) l r
    | r-l == ln = a
    | ls /= rs = segSum cl l (rs*hl) <> segSum cr (rs*hl) r
    | even ls = segSum cl l r
    | otherwise = segSum cr l r
    where
        hl = ln`div`2
        ls = l`div`hl
        rs = (r-1)`div`hl
newtype DualSeg a = DualSeg (Seg a)
instance HasField "len" (DualSeg a) Int where
    getField (DualSeg (SegLeaf _)) = 1
    getField (DualSeg (SegNode l _ _ _)) = l
instance HasField "value" (DualSeg a) a where
    getField (DualSeg (SegLeaf a)) = a
    getField (DualSeg (SegNode _ a _ _)) = a
dualSegMk :: Monoid m => [m] -> DualSeg m
dualSegMk xs = DualSeg $ mk (map SegLeaf $ xs ++ replicate (least2pow xs.len - xs.len) mempty) where
    mk [s] = s
    mk ss = mk (gp ss)
    gp [] = []
    gp (a:b:s) = SegNode (a.len*2) mempty a b : gp s
    gp _ = undefined
dualSegMod :: (m -> m) -> Int -> Int -> DualSeg m -> DualSeg m
dualSegMod f l0 r0 (DualSeg seg) = DualSeg $ go l0 r0 seg where
    go _ _ (SegLeaf a) = SegLeaf (f a)
    go l r (SegNode s a cl cr)
        | l==r = SegNode s a cl cr
        | r-l == s = SegNode s (f a) cl cr
        | ls/=rs = SegNode s a (go l (rs*hl) cl) (go (rs*hl) r cr)
        | even ls = SegNode s a (go l r cl) cr
        | otherwise = SegNode s a cl (go l r cr)
        where
            hl = s`div`2
            ls = l`div`hl
            rs = (r-1)`div`hl
dualSegGet :: Monoid m => Int -> DualSeg m -> m
dualSegGet i0 (DualSeg seg) = go i0 seg where
    go _ (SegLeaf a) = a
    go i (SegNode s a cl cr)
        | even (i`div`(s`div`2)) = go i cl <> a
        | otherwise = a <> go i cr
dualSegToList :: Monoid m => DualSeg m ->[m]
dualSegToList (DualSeg seg) = go mempty seg where
    go acc (SegLeaf a) = [acc <> a]
    go acc (SegNode _ a cl cr) = go (acc <> a) cl ++ go (acc <> a) cr
