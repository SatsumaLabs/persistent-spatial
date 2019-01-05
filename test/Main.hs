{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Morton
import Data.Word
import Control.Lens
import Data.Maybe
import Numeric
import Data.List
import Text.Read

instance (Arbitrary Morton) where
    arbitrary = fmap Morton arbitrary
instance (Arbitrary a, Ord a) => (Arbitrary (Interval a)) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Interval (min x y) (max x y)
instance (Arbitrary MortonRect) where
    arbitrary = MortonRectSides <$> arbitrary <*> arbitrary

prop_morton_isom :: Word32 -> Word32 -> Bool
prop_morton_isom x y = let
    z = MortonPair x y
    MortonPair a b = z
    in (x == a) && (y == b)

prop_morton_isom_rev :: Morton -> Bool
prop_morton_isom_rev z = let
    MortonPair x y = z
    z' = MortonPair x y
    in z == z'

prop_morton_parse :: Morton -> Bool
prop_morton_parse x = readMaybe (show x) == Just x

prop_morton_intersect :: Interval Word32 -> Interval Word32 -> Interval Word32 -> Interval Word32 -> Property
prop_morton_intersect a b c d = let
    rx = MortonRectSides a b
    ry = MortonRectSides c d
    rz = intersectMorton rx ry
    rz' = MortonRectSides <$> (intersectInterval a c) <*> (intersectInterval b d)
    in classify (isJust rz) "overlapping" $ rz == rz'

prop_morton_tile_parse :: Morton -> Property
prop_morton_tile_parse m = forAll (choose (0,64)) $ \n -> let
    t = MortonTile m n
    mt = readMaybe (show t)
    in mt == Just t

prop_morton_tile_bounds :: MortonRect -> Bool
prop_morton_tile_bounds rect = let
    t@(MortonTile _ n) = enclosingMortonTile rect
    s = intervalSizeMorton (mortonTileBounds t)
    s' = 2 ^ (64 - n)
    in s == s'

mortonRectAspect :: MortonRect -> Double
mortonRectAspect (MortonRectSides ix iy) = let
    dx = fromIntegral (intervalSize ix)
    dy = fromIntegral (intervalSize iy)
    in max dx dy / min dx dy

mortonRectExpansion :: Int -> MortonRect -> Word64
mortonRectExpansion subdiv rect = let
    tiles = mortonTileCover subdiv rect
    rsize = mortonRectSize rect
    tsize = sum . fmap (intervalSizeMorton . mortonTileBounds) $ tiles
    -- need to deal with overflow here
    in if tsize == 0 then (2 ^ 63) `div` (rsize `div` 2) else tsize `div` rsize

prop_morton_split_8 :: MortonRect -> Property
prop_morton_split_8 rect = let
    ratio = mortonRectExpansion 3 rect
    in collect ratio $ mortonRectAspect rect < 8 ==> (ratio >= 1 && ratio < 17)

badSquare = MortonRect (Morton 0x0fffFfffFfffFfff) (Morton 0xc000000000000000)

main :: IO ()
main = hspec $ do
    describe "Morton" $ do
        prop "interleaving is reversible" prop_morton_isom
        prop "deinterleaving is reversible" prop_morton_isom_rev
        prop "parsing works" prop_morton_parse
        prop "rectangle intersection mathes interval intersection" prop_morton_intersect
    describe "Morton Cover" $ do
        prop "parsing works" prop_morton_tile_parse
        prop "tile bounds size matches mask value" prop_morton_tile_bounds
        it   "pathological square expands at most 11-fold in 4-division" $ mortonRectExpansion 2 badSquare `shouldSatisfy` (< 11)
        it   "pathological square expands at most 6-fold in 8-division" $ mortonRectExpansion 3 badSquare `shouldSatisfy` (< 6)
        modifyMaxSuccess (const 100000) $ prop "rectangle expansion in 8 tile coder (eccentricity limit 8)" prop_morton_split_8
