{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Prelude hiding (print)
import           Control.Applicative ((<|>))
import           Control.Arrow ((***), (&&&))
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
import           Control.Monad.Trans.Cont (ContT(ContT, runContT), evalContT, Cont, cont, runCont, evalCont, shiftT)
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import           Control.Monad.Writer.Class as Writer
import           Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, execWriterT, Writer, runWriter, execWriter)
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Bifunctor as BF
import           Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Internal (c2w, w2c, isSpaceWord8, isSpaceChar8)
import           Data.ByteString.Builder as BB
import           Data.ByteString.Builder.Internal as BB
import           Data.Char (intToDigit, digitToInt, isSpace, isDigit)
import           Data.Functor.Identity (Identity(runIdentity))
import           Data.Foldable (foldrM, foldlM, toList, traverse_, maximumBy, minimumBy, foldl')
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
import           Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef (STRef, newSTRef, readSTRef, modifySTRef, writeSTRef)
import           Data.Traversable (traverse, mapAccumR, mapAccumL)
import           Data.Tuple.Extra (both)
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Algorithms.Heap as VH
import qualified Data.Vector.Mutable as MV
import           Data.Word (Word64)
import           Debug.Trace (trace)
import           GHC.Records (HasField(..))
import           System.IO (hFlush, stdout)
import           Text.Printf (printf)



main = runBuffered $ do
    n <- readInt
    return ()


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

class Output a where
    toBs :: a -> BB.Builder
instance Output Int where
    toBs = BB.intDec
instance Output Double where
    toBs = BB.doubleDec
instance (Output a, Output b) => Output (a, b) where
    toBs (a, b) = toBs a <> " " <> toBs b
instance Output Char where
    toBs = BB.char7
instance Output String where
    toBs = BB.string7
instance Output ByteString where
    toBs = BB.byteString
instance Output a => Output (V.Vector a) where
    toBs v = foldl (<>) BB.empty $! V.generate (v.len * 2 - 1) (\i -> if even i then toBs (v.at (div i 2)) else BB.char7 ' ')
instance Output a => Output [a] where
    toBs l = foldl (<>) BB.empty $! L.intersperse (BB.char7 ' ') $! map toBs l
data FOutput f a = FO
    { showCnt :: Bool
    , separator :: ByteString
    , values :: f a
    }
instance (Output a, Foldable f) => Output (FOutput f a) where
    toBs (FO sc sep vs) =
        let bd = foldl (<>) BB.empty $! L.intersperse (BB.byteString sep) (toBs <$> toList vs)
        in if sc then toBs (length vs) <> "\n" <> bd else bd
printLines :: (Foldable f, Output a, Atcoder m) => f a -> m ()
printLines = print . FO True "\n"
printWords :: (Foldable f, Output a, Atcoder m) => f a -> m ()
printWords = print . FO True " "

class Atcoder m where
    readInt :: m Int
    readChar :: m Char
    readWord :: m ByteString
    print :: Output a => a -> m ()

readInt1 :: (Functor m, Atcoder m) => m Int
readInt1 = pred <$> readInt
readT :: (Applicative m, Atcoder m) => m (Int, Int)
readT = (,) <$> readInt <*> readInt
readT1 :: (Applicative m, Atcoder m) => m (Int, Int)
readT1 = (,) <$> readInt1 <*> readInt1
toVC :: ByteString -> VC
toVC bs = V.generate (BS8.length bs) (BS8.index bs)
fromVC :: VC -> ByteString
fromVC = BS8.pack . toList
yesno :: Atcoder m => Bool -> m ()
yesno = print . (!!) @ByteString ["No", "Yes"] . fromEnum

dropSpaces :: ByteString -> ByteString
dropSpaces = BS.dropWhile isSpaceWord8

type Buffered = WriterT BB.Builder (StateT ByteString Maybe)
instance Atcoder Buffered where
    readInt = lift . StateT $! BS8.readInt . dropSpaces
    readChar = lift . StateT $! BS8.uncons . dropSpaces
    readWord = lift . StateT $! pure . BS8.break isSpaceChar8 . dropSpaces
    print a = Writer.tell (toBs a <> BB.char7 '\n')

untilEmpty :: (ByteString -> Maybe (a, ByteString)) -> StateT ByteString (MaybeT IO) a
untilEmpty f = do
    s <- State.get
    if BS8.null (dropSpaces s) then do
        s' <- liftIO BS8.getLine
        State.put s'
        untilEmpty f
    else
        StateT $! MaybeT . pure . f . dropSpaces

type Interactive = StateT ByteString (MaybeT IO)
instance Atcoder Interactive where
    readInt = untilEmpty BS8.readInt
    readChar = untilEmpty BS8.uncons
    readWord = untilEmpty (pure . BS8.break isSpaceChar8)
    print a = liftIO $! BB.hPutBuilder stdout (toBs a <> BB.char7 '\n') >> hFlush stdout

runBuffered :: Buffered a -> IO a
runBuffered m = do
    s <- BS8.getContents
    let res = runStateT (runWriterT m) s
    case res of
        Nothing -> error "error occurred"
        Just ((a, out), s) -> do
            BB.hPutBuilder stdout out
            return a

runInteractive :: Interactive a -> IO a
runInteractive m = do
    res <- runMaybeT (runStateT m "")
    case res of
        Nothing -> error "error ocurred"
        Just (a, _) -> do
            return a


-- Type Aliases
type V = V.Vector
type MV = V.MVector
type VC = V.Vector Char
type VI = V.Vector Int
type T = (Int, Int)
type Mat a = V.Vector (V.Vector a)

-- Vector with Ix
data IxV i a = IxV (VU.Vector a) (i, i)
fromListIxV rng l = IxV (VU.fromList (take (rangeSize rng) l)) rng
instance (VU.Unbox a, Ix i) => HasField "at" (IxV i a) (i -> a) where
    getField (IxV v rng) i = v VU.! (index rng i)

data IxMV s i a = IxMV (MVU.MVector s a) (i, i)
newIxMV rng a = IxMV <$> VU.thaw (VU.replicate (rangeSize rng) a) <*> pure rng
instance (VU.Unbox a, Ix i) => HasField "at" (IxMV s i a) (i -> ST s a) where
    getField (IxMV v rng) i = MVU.read v (index rng i)
instance (VU.Unbox a, Ix i) => HasField "set" (IxMV s i a) (i -> a -> ST s ()) where
    getField (IxMV v rng) i a = MVU.write v (index rng i) a
instance (VU.Unbox a, Ix i) => HasField "freeze" (IxMV s i a) (ST s (IxV i a)) where
    getField (IxMV v rng) = IxV <$> VU.freeze v <*> pure rng


-- Dot Access Definitions
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
    getField mv = flip $! MV.modify mv
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

instance HasField "intersperse" (V.Vector a) (a -> V.Vector a) where
    getField v a = V.generate (2*v.len - 1) (\i -> if even i then v.at (i`div`2) else a)

instance HasField "at" ByteString (Int -> Char) where
    getField = BS8.index
instance HasField "len" ByteString Int where
    getField = BS8.length
instance HasField "substr" ByteString (Int -> Int -> ByteString) where
    getField bs i j = BS8.drop i (BS8.take j bs)

instance HasField "slice" (V.Vector a) (Int -> Int -> V.Vector a) where
    getField v l r = V.slice l (r-l) v
instance HasField "swap" (V.Vector a) ((Int, Int) -> (Int, Int) -> V.Vector a) where
    getField v s1 s2 = V.concat [v.slice 0 l0, v.slice l1 r1, v.slice r0 l1, v.slice l0 r0, v.slice r1 v.len] where
        (l0, r0) = min s1 s2
        (l1, r1) = max s1 s2

instance HasField "digits" Int VI where
    getField d = V.iterateN d (`shiftL`1) 1




-- Algorithms
onMat w h (x, y) = 0 <= x && x < w && 0 <= y && y < h

lowerBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
lowerBound lower upper sat = go (lower-1) (upper+1) where
    go ok ng
        | abs (ok-ng) > 1 = let mid = ok+((ng-ok)`div`2) in if sat mid then go mid ng else go ok mid
        | otherwise = if ok == lower-1 then Nothing else Just ok
upperBound :: Int -> Int -> (Int -> Bool) -> Maybe Int
upperBound lower upper sat = go (lower-1) (upper+1) where
    go ng ok
        | abs (ok-ng) > 1 = let mid = ng+((ok-ng)`div`2) in if sat mid then go ng mid else go mid ok
        | otherwise = if ok == upper+1 then Nothing else Just ok
---- cumulative
instance HasField "cumulative" (V.Vector a) ((a -> a -> a) -> a -> V.Vector a) where
    getField v f d = ($! v) $! uncurry (flip V.snoc) . mapAccumL (\s a -> (f s a, s)) d
cumulative2d :: (a -> a -> a) -> a -> Mat a -> Mat a
cumulative2d f d mat = uncurry (flip V.snoc) $! mapAccumL (\s a -> (V.zipWith f s a, s)) (V.replicate (V.length (hs!0)) d) hs where
    hs = V.map (\v -> v.cumulative f d) mat
---- UnionFind
newtype UnionFind s = UnionFind (MV.MVector s Int, MV.MVector s Int, MV.MVector s Int)
newUF :: Int -> ST s (UnionFind s)
newUF n = do
    par <- V.thaw $! V.iterateN n (+1) 0
    rank <- V.thaw $! V.replicate n 0
    siz <- V.thaw $! V.replicate n 1
    return $! UnionFind (par, rank, siz)
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
instance HasField "unite" (UnionFind s) (Int -> Int -> ST s Bool) where
    getField uf@(UnionFind (par, rank, _)) x y = do
        (rx, ry) <- (,) <$> uf.root x <*> uf.root y
        if rx == ry then
            return False
        else do
            cr <- (<) <$> MV.read rank rx <*> MV.read rank ry
            let (rx', ry') = if cr then (ry, rx) else (rx, ry)
            MV.write par ry' rx'
            whenM ((==) <$> MV.read rank rx <*> MV.read rank ry) $! MV.modify rank (+1) rx'
            return True
instance HasField "size" (UnionFind s) (Int -> ST s Int) where
    getField (UnionFind(_, _, siz)) = MV.read siz

-- search
class PushPop pp where
    type Elem pp
    push :: Elem pp -> pp -> pp
    pop :: pp -> Maybe (Elem pp, pp)
searchM :: forall i m c pp. (Ix i, PrimMonad m, PushPop pp, VU.Unbox c, Eq c) => IxMV (PrimState m) i c -> (Elem pp -> m [Elem pp]) -> (Elem pp -> (i, c)) -> pp -> m ()
searchM tbl@(IxMV _ rng) f ix ini = do
    go tbl ini
    where
    go tbl pp = case pop pp of
        Nothing -> pure ()
        Just (e, pp') -> do
            let (i, c) = ix e
            d <- liftPrim $ tbl.at i
            if d == c then go tbl pp'
            else do
                nx <- f e
                liftPrim $ tbl.set i c
                go tbl (foldl' (flip push) pp' $ filter (inRange rng . fst . ix) nx)
searchM' :: forall i m pp. (Ix i, PrimMonad m, PushPop pp) => (i, i) -> (Elem pp -> m [Elem pp]) -> (Elem pp -> i) -> pp -> m ()
searchM' rng f ix ini = do
    tbl <- liftPrim @(ST (PrimState m)) $ newIxMV rng False
    searchM tbl f ((,True) . ix) ini
searchFindM :: (Ix i, PrimMonad m, PushPop pp) => (i, i) -> (Elem pp -> m (Either r [Elem pp])) -> (Elem pp -> i) -> pp -> m (Maybe r)
searchFindM rng f ix = fmap (either Just (const Nothing)) . runExceptT . searchM' rng (ExceptT . f) ix
searchFind :: (Ix i, PushPop pp) => (i, i) -> (Elem pp -> Either r [Elem pp]) -> (Elem pp -> i) -> pp -> Maybe r
searchFind rng f ix ini = runST $ searchFindM rng (pure . f) ix ini

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
        | otherwise = Just $! Set.deleteFindMax st


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
    (Mint m) + (Mint n) = Mint $! (m + n) `mod` mintMod
    (Mint m) * (Mint n) = Mint $! (m * n) `mod` mintMod
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
least2pow n = head . filter (n<=) $ iterate (*2) 1
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
