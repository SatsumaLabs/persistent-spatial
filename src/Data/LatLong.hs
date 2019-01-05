{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, PatternSynonyms #-}

{-|
Module: Data.LatLong
Description: Spatially indexed type for geographic coordinates.
-}

module Data.LatLong (
    LatLong(..), lat, long,
    geoDistance,
    --latLongZCover,
) where

import Data.Morton
import Data.Aeson
import Data.Monoid
import Control.Monad
import Control.Lens (Lens', Iso', iso)
import Data.Word (Word32)
import Debug.Trace
import Web.HttpApiData
import qualified Data.Text as T
import Numeric


newtype LatLong = LatLongZ Morton deriving (Eq, Ord)

two32f :: Double
two32f = 2 ^ (32 :: Int)


makeLatLong :: Double -> Double -> LatLong
makeLatLong theta phi = LatLongZ (MortonPair theta' phi') where
    theta' = clampLat . floor $ (theta + 90) / 360 * two32f
    phi' = wrapLong . floor $ (phi + 180) / 360 * two32f
    clampLat (x::Int) = fromIntegral (max 0 (min 0x7fffffff x))
    wrapLong (x::Int) = fromIntegral (x `mod` 0x100000000)

latLongCoords :: LatLong -> (Double, Double)
latLongCoords (LatLongZ (MortonPair theta' phi')) = (theta,phi) where
    theta = (fromIntegral theta' * 360 / two32f) - 90
    phi = (fromIntegral phi' * 360 / two32f) - 180

pattern LatLong :: Double -> Double -> LatLong
pattern LatLong theta phi <- (latLongCoords -> (theta,phi)) where
    LatLong theta phi = makeLatLong theta phi

{-# COMPLETE LatLong #-}


instance ToJSON LatLong where
    toJSON (LatLong theta phi) = object ["lat" .= theta, "long" .= phi]
    toEncoding (LatLong theta phi) = pairs $ "lat" .= theta <> "long" .= phi
instance FromJSON LatLong where
    parseJSON = withObject "LatLong" $ \o -> do
        theta <- o .: "lat"
        phi <- o .: "long"
        guard $ theta > -90 && theta < 90
        guard $ phi >= -180 && phi < 180
        return $ LatLong theta phi

instance FromHttpApiData LatLong where
    parseUrlPiece s = maybe (Left "malformed coordinate pair") Right $ do
        [theta',phi'] <- return $ T.splitOn "," s
        Right theta <- return $ parseUrlPiece theta'
        Right phi <- return $ parseUrlPiece phi'
        guard $ theta > -90 && theta < 90
        guard $ phi >= -180 && phi < 180
        return $ LatLong theta phi
instance ToHttpApiData LatLong where
    toUrlPiece (LatLong theta phi) = toUrlPiece theta <> "," <> toUrlPiece phi

instance Show LatLong where
    show (LatLong theta phi) = join
        [ showFFloat (Just 5) (abs theta) []
        , if theta >= 0 then " N " else " S "
        , showFFloat (Just 5) (abs phi) []
        , if phi >= 0 then " E" else " W" ]


lat :: Lens' LatLong Double
lat f (LatLong theta phi) = fmap (\theta' -> LatLong theta' phi) (f theta)
long :: Lens' LatLong Double
long f (LatLong theta phi) = fmap (\phi' -> LatLong theta phi') (f phi)


earthRadius :: Double
earthRadius = 6371.2e3

rads x = x / 180 * pi
degs x = x * pi / 180
sindeg = sin . rads
cosdeg = cos . rads

-- | Calculate distance between two points using the Haversine formula (up to 0.5% due to the eaaumption of a spherical Earth).
-- Distance is returned in metres.
geoDistance :: LatLong -> LatLong -> Double
geoDistance (LatLong theta1 phi1) (LatLong theta2 phi2) = earthRadius * sigma where
    havdelta x y = sindeg ((x-y)/2) ^ (2::Int)
    hav = havdelta theta1 theta2 + (cosdeg theta1 * cosdeg theta2 * havdelta phi1 phi2)
    sigma = 2 * asin (sqrt hav)




{-
latLongZCover :: LatLong -> Double -> [Interval LatLong]
latLongZCover (LatLong theta phi) r = (fmap.fmap) LatLongZ mranges where
    plusminus a b = (a-b,a+b)
    (thetaa,thetab) = plusminus theta $ degs (r / earthRadius)
    (phia, phib) = plusminus phi $ degs (r / earthRadius / cosdeg theta)
    LatLongZ (MortonPair lata longa) = LatLong thetaa phia
    LatLongZ (MortonPair latb longb) = LatLong thetab phib
    mranges = mortonCover (Interval lata latb) (Interval longa longb)
-}
