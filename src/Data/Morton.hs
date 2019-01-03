{-# LANGUAGE BangPatterns, ScopedTypeVariables, DeriveFunctor, ViewPatterns, PatternSynonyms #-}

{- |
Module: Data.Morton
Description: Morton reperesention of integer pairs

Morton reperesentation of integer pairs (interleaved bits) used for creating spatial indexes.
Some parts taken from the documentation of Data.Sparse (code no longer in current version) at
https://www.schoolofhaskell.com/user/edwardk/revisiting-matrix-multiplication/part-1
-}

module Data.Morton (
    Morton(Morton,MortonPair), _Morton,
    Interval(..), mortonCover,
) where

import Data.Bits
import Data.Word
import Control.Lens
import Numeric.Lens
import Numeric
import Data.Monoid ((<>))
import Data.Maybe


newtype Morton = Morton Word64 deriving (Eq, Ord)
instance (Show Morton) where
    show (Morton m) = "Zx" <> showHex m []

-- interleaves the bits of two integers by performing AH,AL,BH,BL -> AH,BH,AL,BL then recursing on H and L portions
interleaveM :: Word32 -> Word32 -> Morton
interleaveM !x !y = Morton k5 where
    k0 = unsafeShiftL (fromIntegral x) 32 .|. fromIntegral y
    -- interleaves the bits of two integers by performing AH,AL,BH,BL -> AH,BH,AL,BL then recursing on H and L portions
    k1 = unsafeShiftL (k0 .&. 0x00000000FFFF0000) 16  .|. unsafeShiftR k0 16 .&. 0x00000000FFFF0000  .|. k0 .&. 0xFFFF00000000FFFF
    k2 = unsafeShiftL (k1 .&. 0x0000FF000000FF00) 8   .|. unsafeShiftR k1 8  .&. 0x0000FF000000FF00  .|. k1 .&. 0xFF0000FFFF0000FF
    k3 = unsafeShiftL (k2 .&. 0x00F000F000F000F0) 4   .|. unsafeShiftR k2 4  .&. 0x00F000F000F000F0  .|. k2 .&. 0xF00FF00FF00FF00F
    k4 = unsafeShiftL (k3 .&. 0x0C0C0C0C0C0C0C0C) 2   .|. unsafeShiftR k3 2  .&. 0x0C0C0C0C0C0C0C0C  .|. k3 .&. 0xC3C3C3C3C3C3C3C3
    k5 = unsafeShiftL (k4 .&. 0x2222222222222222) 1   .|. unsafeShiftR k4 1  .&. 0x2222222222222222  .|. k4 .&. 0x9999999999999999


uninterleaveM :: Morton -> (Word32,Word32)
uninterleaveM (Morton !k0) = (fromIntegral (unsafeShiftR k5 32), fromIntegral (k5 .&. 0x00000000FFFFFFFF)) where
    k5 = unsafeShiftL (k4 .&. 0x00000000FFFF0000) 16  .|. unsafeShiftR k4 16 .&. 0x00000000FFFF0000  .|. k4 .&. 0xFFFF00000000FFFF
    k4 = unsafeShiftL (k3 .&. 0x0000FF000000FF00) 8   .|. unsafeShiftR k3 8  .&. 0x0000FF000000FF00  .|. k3 .&. 0xFF0000FFFF0000FF
    k3 = unsafeShiftL (k2 .&. 0x00F000F000F000F0) 4   .|. unsafeShiftR k2 4  .&. 0x00F000F000F000F0  .|. k2 .&. 0xF00FF00FF00FF00F
    k2 = unsafeShiftL (k1 .&. 0x0C0C0C0C0C0C0C0C) 2   .|. unsafeShiftR k1 2  .&. 0x0C0C0C0C0C0C0C0C  .|. k1 .&. 0xC3C3C3C3C3C3C3C3
    k1 = unsafeShiftL (k0 .&. 0x2222222222222222) 1   .|. unsafeShiftR k0 1  .&. 0x2222222222222222  .|. k0 .&. 0x9999999999999999


_Morton :: Iso' Morton (Word32,Word32)
_Morton = iso uninterleaveM (uncurry interleaveM)

pattern MortonPair :: Word32 -> Word32 -> Morton
pattern MortonPair x y <- (uninterleaveM -> (x,y)) where
    MortonPair x y = interleaveM x y

{-# COMPLETE MortonPair #-}



data Interval a = Interval a a deriving (Eq, Show, Functor)
data MRect = MRect {-# UNPACK #-} !Morton {-# UNPACK #-} !Morton deriving (Eq, Show)

makeRect :: Interval Word32 -> Interval Word32 -> MRect
makeRect (Interval ax bx) (Interval ay by) = MRect (interleaveM ax ay) (interleaveM bx by)

intersectM :: MRect -> MRect -> Maybe MRect
intersectM (MRect a1 b1) (MRect a2 b2) = mint where
    selx (Morton m) = m .&. 0xAAAAAAAAAAAAAAAA
    sely (Morton m) = m .&. 0x5555555555555555
    -- interleaving bits with 0 preserves ordering so bit shifts are not necesary for comparisons
    ax = max (selx a1) (selx a2)
    ay = max (sely a1) (sely a2)
    bx = min (selx b1) (selx b2)
    by = min (sely b1) (sely b2)
    mint = if (ax <= bx) && (ay <= by) then Just (MRect (Morton $ ax .|. ay) (Morton $ bx .|. by)) else Nothing

rectSize :: Morton -> Morton -> Word64
rectSize a b = fromIntegral (bx-ax+1) * fromIntegral (by-ay+1) where
    (ax,ay) = uninterleaveM a
    (bx,by) = uninterleaveM b
rangeSize :: Morton -> Morton -> Word64
rangeSize (Morton a) (Morton b) = b - a + 1

splitCover :: MRect -> (MRect,MRect)
splitCover (MRect (Morton a) (Morton b)) = (MRect (Morton low) (Morton mid), MRect (Morton (mid+1)) (Morton high)) where
    mask = unsafeShiftR 0xFFFFFFFFFFFFFFFF . countLeadingZeros $ a `xor` b
    low = a .&. complement mask
    mid = low + unsafeShiftR mask 1
    high = b .|. mask

splitCoverMin :: MRect -> (MRect, MRect)
splitCoverMin r = (fromJust $ intersectM r lc, fromJust $ intersectM r uc)
    where (lc, uc) = splitCover r

rectToRanges :: Int -> Word64 -> MRect -> [Interval Morton]
rectToRanges thresh1 thresh2 r@(MRect a b)
    | rsize >= isize `div` fromIntegral thresh1 || rsize + thresh2 > isize = [Interval a b]
    | otherwise = recurse rx ++ recurse ry
    where
        isize = rangeSize a b
        rsize = rectSize a b
        (rx, ry) = splitCoverMin r
        recurse r' = rectToRanges thresh1 (thresh2 `div` 2) r'


-- | Calculates a 'reasonable' cover of morton ranges for a rectangle on the torus @(Word32, Word32)@
mortonCover :: Interval Word32 -> Interval Word32 -> [Interval Morton]
mortonCover (Interval ax bx) (Interval ay by) = initrects >>= rectToRanges 4 t where
    dx = bx-ax
    dy = by-ay
    area :: Word64 = fromIntegral dx * fromIntegral dy
    initrects = makeRect <$> (if bx >= ax then [Interval ax bx] else [(Interval ax maxBound), (Interval minBound bx)])
                         <*> (if by >= ay then [Interval ay by] else [(Interval ay maxBound), (Interval minBound by)])
    t = (3*area) `div` fromIntegral (length initrects)
