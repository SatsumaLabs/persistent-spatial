{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Lens
import Data.Word
import Data.List
import Data.Maybe
import Data.Aeson
import Text.Read
import Numeric
import Web.HttpApiData
import Database.Persist

import Data.Morton
import Data.LatLong

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
    sr = mortonRectSize (mortonTileRect t)
    s' = 2 ^ (64 - n)
    in s == s' && sr == s'


mortonRectAspect :: MortonRect -> Double
mortonRectAspect (MortonRectSides ix iy) = let
    dx = intervalSize ix
    dy = intervalSize iy
    in fromIntegral (max dx dy) / fromIntegral (min dx dy)

mortonRectExpansion :: MortonRect -> Integer
mortonRectExpansion rect = let
    tiles = mortonTileCover rect
    rsize = mortonRectSize rect
    tsize = sum . fmap (intervalSizeMorton . mortonTileBounds) $ tiles
    in (tsize + 1) `div` rsize

prop_morton_split_8 :: MortonRect -> Property
prop_morton_split_8 rect = let
    ratio = mortonRectExpansion rect
    in collect ratio $ mortonRectAspect rect < 8 ==> (ratio >= 1 && ratio <= 16)

badSquare = MortonRect (Morton 0x0fffFfffFfffFfff) (Morton 0xc000000000000000)



instance (Arbitrary LatLong) where
    arbitrary = LatLong <$> choose (-90,90) <*> choose (-180,180)

prop_latlong_json :: LatLong -> Bool
prop_latlong_json p = let
    mp = decode (encode p)
    in mp == Just p

prop_latlong_http :: LatLong -> Bool
prop_latlong_http p = let
    mp = parseUrlPiece (toUrlPiece p)
    in mp == Right p

prop_latlong_persist :: LatLong -> Bool
prop_latlong_persist p = let
    mp = fromPersistValue (toPersistValue p)
    in mp == Right p

prop_geo_triangle :: LatLong -> LatLong -> LatLong -> Bool
prop_geo_triangle a b c = let
    ttest x y z = geoDistance x y + geoDistance y z >= geoDistance x z
    in ttest a b c && ttest b c a && ttest c a b

pctError :: Double -> Double -> Double
pctError ref samp = abs (samp - ref) / ref

prop_square_corner_dist :: Property
prop_square_corner_dist = let
    gen = (,) <$> choose (10,200000) <*> (LatLong <$> choose (-70,70) <*> choose (-180,180))
    test (r,p) = let
        d = 2*r
        (LatLong s w, LatLong n e) = geoSquare p r
        laterr =  max (pctError d (geoDistance (LatLong n w) (LatLong s w)))
                      (pctError d (geoDistance (LatLong n e) (LatLong s e)))
        --laterrbin :: Int = max (-9) (ceiling (logBase 10 laterr))
        longerr = max (pctError d (geoDistance (LatLong n w) (LatLong n e)))
                      (pctError d (geoDistance (LatLong s w) (LatLong s e)))
        longerrbin = minimum [x | x <- [100,10,5,2,1,0.5,0.1,0.01], x > longerr * 100]
        in collect longerrbin . counterexample (show (laterr,longerr)) $ laterr < 1e-3 && longerr < 0.1
    in forAll gen test

prop_tiles_cover_points :: Property
prop_tiles_cover_points = let
    gen = (,) <$> choose (10,1000000) <*> (LatLong <$> choose (-70,70) <*> choose (-180,180))
    test (r,p) = let
        (sw@(LatLong s w), ne@(LatLong n e)) = geoSquare p r
        tiles = latLongTileCover sw ne
        pgen = LatLong <$> choose (s,n) <*> choose (w,e)
        in forAll pgen (`tileSetElem` tiles)
    in forAll gen test

main :: IO ()
main = hspec . modifyMaxSuccess (const 10000) $ do
    describe "Data.Morton" $ do
        prop "interleaving is reversible" prop_morton_isom
        prop "deinterleaving is reversible" prop_morton_isom_rev
        prop "point Read instance works" prop_morton_parse
        prop "rectangle intersection mathes interval intersection" prop_morton_intersect
        prop "tile Read instance works" prop_morton_tile_parse
        prop "tile size (both definitions) matches mask value" prop_morton_tile_bounds
        it   "pathological square expands at most 6-fold" $ mortonRectExpansion badSquare `shouldSatisfy` (< 6)
        modifyMaxSuccess (const 100000) $ prop "rectangle expansion tile cover (eccentricity limit 8, floored expansion collected)" prop_morton_split_8
    describe "Data.LatLong" $ do
        prop "Aeson instances work" prop_latlong_json
        prop "HttpApiData instances work" prop_latlong_http
        prop "PersistField instance works" prop_latlong_persist
        prop "geoDistance obeys triangle inequality" prop_geo_triangle
        prop "geoSquare corners within tolerance (% error collected), 10m < r < 200km, 70S < lat < 70N" prop_square_corner_dist
        prop "tile covers contain all points in square" prop_tiles_cover_points
