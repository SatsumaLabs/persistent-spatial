{-# LANGUAGE BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveFunctor, ViewPatterns, PatternSynonyms #-}

{- |
Module: Data.Morton
Description: Morton reperesention of integer pairs

Morton reperesentation of integer pairs (interleaved bits) used for creating spatial indexes.

Bit interleaving code is originally from the documentation of Data.Sparse by Edward Kmett at
<https://www.schoolofhaskell.com/user/edwardk/revisiting-matrix-multiplication/part-1>
-}

module Data.Morton (
    Morton(Morton,MortonPair),
    Interval(..), intersectInterval, intervalElem, intervalSize, intervalSizeMorton,
    MortonRect(MortonRect,MortonRectSides), mortonRectBounds, intersectMorton, mortonRectSize,
    MortonTile(..), mortonTileBounds, mortonTileRect, enclosingMortonTile, splitMortonTile, trimMortonTile,
    mortonTileCover, mortonTileCoverTorus,
    --mortonCover,
) where

import Data.Bits
import Data.Word
import Control.Lens
import Numeric.Lens
import Numeric
import Data.Monoid ((<>))
import Data.Maybe
import Data.Ord
import Control.Monad
import Text.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.ParserCombinators.ReadP
import Math.NumberTheory.Logarithms

zeropad :: Int -> String -> String
zeropad n s = replicate (n - length s) '0' ++ s

-- | Type implementing a Morton Z-Order Curve.
-- Stores two 'Word32' values with bits interleaved for spatial indexing by rectangular tiles which form contiguous intervals.
newtype Morton = Morton Word64 deriving (Eq, Ord, Enum)
instance (Show Morton) where
    show (Morton m) = "Z" <> zeropad 16 (showHex m [])
instance (Read Morton) where
    readPrec = readP_to_Prec (const readMorton)

readMorton :: ReadP Morton
readMorton = char 'Z' >> fmap Morton readHexP


-- interleaves the bits of two integers by performing AH,AL,BH,BL -> AH,BH,AL,BL then recursing on H and L portions in SIMD fashion
interleaveM :: Word32 -> Word32 -> Morton
interleaveM !x !y = Morton k5 where
    k0 = unsafeShiftL (fromIntegral x) 32 .|. fromIntegral y
    k1 = unsafeShiftL (k0 .&. 0x00000000FFFF0000) 16  .|. unsafeShiftR k0 16 .&. 0x00000000FFFF0000  .|. k0 .&. 0xFFFF00000000FFFF
    k2 = unsafeShiftL (k1 .&. 0x0000FF000000FF00) 8   .|. unsafeShiftR k1 8  .&. 0x0000FF000000FF00  .|. k1 .&. 0xFF0000FFFF0000FF
    k3 = unsafeShiftL (k2 .&. 0x00F000F000F000F0) 4   .|. unsafeShiftR k2 4  .&. 0x00F000F000F000F0  .|. k2 .&. 0xF00FF00FF00FF00F
    k4 = unsafeShiftL (k3 .&. 0x0C0C0C0C0C0C0C0C) 2   .|. unsafeShiftR k3 2  .&. 0x0C0C0C0C0C0C0C0C  .|. k3 .&. 0xC3C3C3C3C3C3C3C3
    k5 = unsafeShiftL (k4 .&. 0x2222222222222222) 1   .|. unsafeShiftR k4 1  .&. 0x2222222222222222  .|. k4 .&. 0x9999999999999999


uninterleaveM :: Morton -> (Word32,Word32)
uninterleaveM (Morton k0) = (fromIntegral (unsafeShiftR k5 32), fromIntegral (k5 .&. 0x00000000FFFFFFFF)) where
    k5 = unsafeShiftL (k4 .&. 0x00000000FFFF0000) 16  .|. unsafeShiftR k4 16 .&. 0x00000000FFFF0000  .|. k4 .&. 0xFFFF00000000FFFF
    k4 = unsafeShiftL (k3 .&. 0x0000FF000000FF00) 8   .|. unsafeShiftR k3 8  .&. 0x0000FF000000FF00  .|. k3 .&. 0xFF0000FFFF0000FF
    k3 = unsafeShiftL (k2 .&. 0x00F000F000F000F0) 4   .|. unsafeShiftR k2 4  .&. 0x00F000F000F000F0  .|. k2 .&. 0xF00FF00FF00FF00F
    k2 = unsafeShiftL (k1 .&. 0x0C0C0C0C0C0C0C0C) 2   .|. unsafeShiftR k1 2  .&. 0x0C0C0C0C0C0C0C0C  .|. k1 .&. 0xC3C3C3C3C3C3C3C3
    k1 = unsafeShiftL (k0 .&. 0x2222222222222222) 1   .|. unsafeShiftR k0 1  .&. 0x2222222222222222  .|. k0 .&. 0x9999999999999999

-- | Construct a Morton value from its two coordinates.
pattern MortonPair :: Word32 -> Word32 -> Morton
pattern MortonPair x y <- (uninterleaveM -> (x,y)) where
    MortonPair x y = interleaveM x y
{-# COMPLETE MortonPair #-}


-- | Type for closed intervals. The second field should be greater than the first.
data Interval a = Interval a a deriving (Eq, Show, Read, Functor)

-- | Returns intersection of two intervals, or Nothing if they do not overlap
intersectInterval :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersectInterval (Interval a b) (Interval a' b') = do
    guard (a <= b)
    guard (a' <= b')
    let x = max a a'
        y = min b b'
    guard (x <= y)
    return $ Interval x y

intervalElem :: (Ord a) => a -> Interval a -> Bool
intervalElem x (Interval a b) = a <= x && x <= b

-- | returns the size of an integer interval
intervalSize :: (Integral a) => Interval a -> Integer
intervalSize (Interval a b) = fromIntegral b - fromIntegral a + 1

-- | Returns the size of a Morton interval. This is necesary as Morton is too large to use the Enum typeclass (which has a 63 bit limit for its size functions)
intervalSizeMorton :: Interval Morton -> Integer
intervalSizeMorton (Interval (Morton a) (Morton b)) = fromIntegral b - fromIntegral a + 1


-- | Type for retangles in Morton space reperesented by upper-left and lower-right corners
data MortonRect = MortonRect {-# UNPACK #-} !Morton {-# UNPACK #-} !Morton deriving (Eq, Show, Read)

-- | returns x,y bounds of a rectangle
mortonRectBounds :: MortonRect -> (Interval Word32, Interval Word32)
mortonRectBounds (MortonRect (MortonPair x y) (MortonPair x' y')) = (Interval x x', Interval y y')

-- | Construct/match rectangles by  their sides
pattern MortonRectSides :: Interval Word32 -> Interval Word32 -> MortonRect
pattern MortonRectSides xs ys <- (mortonRectBounds -> (xs,ys)) where
    MortonRectSides (Interval x x') (Interval y y') = MortonRect (MortonPair x y) (MortonPair x' y')
{-# COMPLETE MortonRectSides #-}

-- | rerurns intersection of two rectangles
intersectMorton :: MortonRect -> MortonRect -> Maybe MortonRect
intersectMorton (MortonRect a1 b1) (MortonRect a2 b2) = mint where
    selx (Morton m) = m .&. 0xAAAAAAAAAAAAAAAA
    sely (Morton m) = m .&. 0x5555555555555555
    -- interleaving bits with 0 preserves ordering so bit shifts are not necesary for comparisons
    ax = max (selx a1) (selx a2)
    ay = max (sely a1) (sely a2)
    bx = min (selx b1) (selx b2)
    by = min (sely b1) (sely b2)
    mint = if (ax <= bx) && (ay <= by) then Just (MortonRect (Morton $ ax .|. ay) (Morton $ bx .|. by)) else Nothing

-- | returns area of rectangle
mortonRectSize :: MortonRect -> Integer
mortonRectSize (MortonRect (MortonPair ax ay) (MortonPair bx by)) =
    (fromIntegral bx - fromIntegral ax + 1) * (fromIntegral by - fromIntegral ay + 1)


-- | Type for a tile in Morton space, which is a special type of rectangle which is the set of all points sharing a common binary prefex.
-- Reperesented as a point and mask length simillarly to a CIDR subnet.
data MortonTile = MortonTile {-# UNPACK #-} !Morton {-# UNPACK #-} !Int
instance (Show MortonTile) where
    show t@(MortonTile _ n) = let Interval m _ = mortonTileBounds t in show m <> "/" <> show n
instance (Read MortonTile) where
    readPrec = readP_to_Prec . const $ do
        m <- readMorton
        char '/'
        n <- readDecP
        guard (n >= 0 && n <= 64)
        return $ MortonTile m n

-- | Values which reperesent the same tile compare equal even if the reperesentative points differ
instance (Eq MortonTile) where
    a == b = mortonTileBounds a == mortonTileBounds b

-- | A tile sorts before its subtiles, i.e. x sorts before 0 and 1
instance (Ord MortonTile) where
    compare = comparing (\x -> let Interval a b = mortonTileBounds x in (a, Down b))

-- | returns a tile as in Interval
mortonTileBounds :: MortonTile -> Interval Morton
mortonTileBounds (MortonTile (Morton x) n) = let
    mask = shiftR 0xFFFFFFFFFFFFFFFF n
    a = x .&. complement mask
    b = x .|. mask
    in Interval (Morton a) (Morton b)

-- | returns a tile as a rectangle
mortonTileRect :: MortonTile -> MortonRect
mortonTileRect t = let
    Interval a b = mortonTileBounds t
    in MortonRect a b

-- | finds the smallest tile completely enclosing a rectangle
--  This can be arbitrarily large if the rectangle crosses a seam.
enclosingMortonTile :: MortonRect -> MortonTile
enclosingMortonTile (MortonRect (Morton a) (Morton b)) =
    let n = countLeadingZeros $ a `xor` b in MortonTile (Morton a) n

-- | Splits a MortonTile in half
splitMortonTile :: MortonTile -> [MortonTile]
splitMortonTile t@(MortonTile _ 64) = [t]
splitMortonTile (MortonTile (Morton m) n) = let
    mask = shiftR 0xFFFFFFFFFFFFFFFF n
    low = m .&. complement mask
    high = m .|. mask
    in [MortonTile (Morton low) (n+1), MortonTile (Morton high) (n+1)]

-- | Trims a @MortonTile@ to the only the subtile overlapping a given rectangle
trimMortonTile :: MortonRect -> MortonTile -> Maybe MortonTile
trimMortonTile rect t = let
    Interval a b = mortonTileBounds t
    trect = MortonRect a b
    irect = intersectMorton rect trect
    in fmap enclosingMortonTile irect

-- | Covers a rectangle using tiles within a range of sizes (specified by their mask values)
mortonTileCoverSized :: Int -> Maybe Int -> MortonRect -> [MortonTile]
mortonTileCoverSized big small rect = let
    expandTile tile@(MortonTile p m) = case small of
        Just s | m > s -> MortonTile p s
        _              -> tile
    subdiv tile@(MortonTile _ m)
        | m >= big = [expandTile tile]
        | otherwise = (>>= subdiv) . mapMaybe (trimMortonTile rect) . splitMortonTile $ tile
    in subdiv (enclosingMortonTile rect)

-- | Covers a rectangle with tiles no larger then the area to be covered (no lower size limit).
-- The total area coverd by these tiles bas a trivial upper bound of 8 tiles the rectangle's area plus the area of its enclosing square
-- and the actual performance is usually (possibly always, although I have not proven so) significantly better.
mortonTileCover :: MortonRect -> [MortonTile]
mortonTileCover rect = let
    big = max 0 $ 64 - integerLog2 (mortonRectSize rect)
    in mortonTileCoverSized big Nothing rect

-- | Version of '@'mortonTileCover'@' which allows the rectangle to wrap around the maximum x/y coordinates (as if the space were a torus).
mortonTileCoverTorus :: Morton -> Morton -> [MortonTile]
mortonTileCoverTorus (MortonPair ax ay) (MortonPair bx by) = let
    rsize = (fromIntegral (bx - ax) + 1) * (fromIntegral (by - ay) + 1) -- integer overflows handle wraparound case
    big = max 0 $ 64 - integerLog2 rsize
    initrects = MortonRectSides
        <$> (if bx >= ax then [Interval ax bx] else [Interval ax maxBound, Interval minBound bx])
        <*> (if by >= ay then [Interval ay by] else [Interval ay maxBound, Interval minBound by])
    in initrects >>= mortonTileCoverSized big Nothing
