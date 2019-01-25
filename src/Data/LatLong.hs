{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, PatternSynonyms #-}

{-|
Module: Data.LatLong
Description: Spatially indexed type for geographic coordinates.
Copyright: © 2018-2019 Satsuma labs

Defines a type for georgraphic coordinates that can be spatially indexed by any database supporting 64 bit integer values.
This indexing works by reperesenting points using a 'Morton' Z-Order curce, with each coordinate reperesented as a 32-bit fixed-point value
which then have their bits interleaved into a 64-bit integer to the internal reperesentation.

Taking binary prefixes of these values divides the globe into a hierarchy of rectangular tiles (repereseteh here as 'LatLongTile' objects),
each of which is a contiguous interval when points are ordered according to their integer reperesentations.
As any geographic region can be covered by a small number of tiles of simillar size, this provides an easy to loop up data for specific reguions.
Instances and a filter for persistent are provided for this purpose.

-}

module Data.LatLong (
    LatLong(LatLongZ, LatLong), lat, long,
    earthRadius, geoDistance, geoSquare,
    -- * Tiles
    LatLongTile, latLongTileInterval,
    latLongTileCover, latLongTileCoverSquare, tileSetElem, withinTileSet
) where

import Data.Morton
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import Control.Monad
import Control.Lens (Lens')
import Data.Word
import Numeric
import Web.HttpApiData
import Database.Persist.Sql

-- | Type for storing geographic coordinates that can be spatially indexed ('Morton' ordering).
-- Each coordinate is reperesented as as 32-bit fixed point value and is also accessible as a Double through a pattern synonym.
-- Order follows a Morton Z-order curve which can be used to search a database by tiles.
-- This works with any database capable of storing and indexing 'Word64' (although this type only uses those values fitting in a 64 bit signed integer)
newtype LatLong =
    -- | Underlying reperesentation and source of ordering for indexing
    LatLongZ Morton
    deriving (Eq, Ord)

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

-- | Pattern for accessing latitide and longitude coordinates as 'Double' values.
-- This is not fully isomoprphic as latitude is clipped to ±90, longitude is wrapped mod 360 ±180,
-- and rounding error exists due to the internal fixed-point reperesentation.
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
        guard $ theta >= -90 && theta <= 90
        guard $ phi >= -180 && phi <= 180
        return $ LatLong theta phi
instance ToHttpApiData LatLong where
    toUrlPiece (LatLong theta phi) = toUrlPiece theta <> "," <> toUrlPiece phi

instance Show LatLong where
    show (LatLong theta phi) = join
        [ showFFloat (Just 5) (abs theta) []
        , if theta >= 0 then " N " else " S "
        , showFFloat (Just 5) (abs phi) []
        , if phi >= 0 then " E" else " W" ]

instance PersistField LatLong where
    toPersistValue (LatLongZ (Morton x)) = toPersistValue x
    fromPersistValue = fmap (LatLongZ . Morton) . fromPersistValue
instance PersistFieldSql LatLong where
    sqlType _ = sqlType (Proxy :: Proxy Word64)


-- | Lens for latitude.
lat :: Lens' LatLong Double
lat f (LatLong theta phi) = fmap (\theta' -> LatLong theta' phi) (f theta)
-- | Lens for longitude.
long :: Lens' LatLong Double
long f (LatLong theta phi) = fmap (\phi' -> LatLong theta phi') (f phi)

-- | Earth's average radius in meters
earthRadius :: Double
earthRadius = 6371.2e3

rads :: Double->Double
rads x = x / 180 * pi
degs :: Double->Double
degs x = x * 180 / pi
sindeg :: Double->Double
sindeg = sin . rads
cosdeg :: Double->Double
cosdeg = cos . rads

-- | Calculate distance between two points using the Haversine formula (up to 0.5% due to the assumption of a spherical Earth).
-- Distance is returned in meters.
geoDistance :: LatLong -> LatLong -> Double
geoDistance (LatLong theta1 phi1) (LatLong theta2 phi2) = earthRadius * sigma where
    havdelta x y = sindeg ((x-y)/2) ^ (2::Int)
    hav = havdelta theta1 theta2 + (cosdeg theta1 * cosdeg theta2 * havdelta phi1 phi2)
    sigma = 2 * asin (sqrt hav)

-- | Calculates the corner coordinates of a square with a given center and radius (in meters).
-- Based on the Mercator projection thus has distortion near the poles
-- (within 5% for a radius at most 200km and latitude within ±70).
geoSquare :: LatLong -> Double -> (LatLong, LatLong)
geoSquare (LatLong theta phi) r = let
    dtheta = degs (r / earthRadius)
    dphi = degs (r / earthRadius / cosdeg theta)
    se = LatLong (theta - dtheta) (phi - dphi)
    nw = LatLong (theta + dtheta) (phi + dphi)
    in (se,nw)





-- | Represents a LatLong tile, which is both a rectangle and a contoguous interval in the ordering.
newtype LatLongTile = LatLongTile MortonTile deriving (Eq, Read, Show)

-- | Gets the corners of a tile, which are also the bounds of its interval in sort order.
latLongTileInterval :: LatLongTile -> Interval LatLong
latLongTileInterval (LatLongTile t) = fmap LatLongZ (mortonTileBounds t)

-- | Covers a rectangle (defined by its corners) tiles of at most its size.
latLongTileCover :: LatLong -> LatLong -> [LatLongTile]
latLongTileCover se nw = let
    LatLong s _ = se
    LatLong n _ = nw
    LatLongZ y = se
    LatLongZ x = nw
    in if s > n then [] else fmap LatLongTile (mortonTileCoverTorus x y)

-- | Covers a square (defined by its center and radius) by tiles.
latLongTileCoverSquare :: LatLong -> Double -> [LatLongTile]
latLongTileCoverSquare c r = uncurry latLongTileCover $ geoSquare c r

-- | Tests whether a point is contasined in a tile set.
tileSetElem :: LatLong -> [LatLongTile] -> Bool
tileSetElem p ts = or [intervalElem p (latLongTileInterval t) | t <- ts]

-- | Persistent filter producing the SQL equiveland ot 'tileSetElem'.
withinTileSet :: (EntityField row LatLong) -> [LatLongTile] -> Filter row
withinTileSet field tiles = let
    tfilter tile = let Interval a b = latLongTileInterval tile in FilterAnd [field >=. a, field <=. b]
    in FilterOr $ fmap tfilter tiles
