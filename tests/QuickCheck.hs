module QuickCheck where

import Test.QuickCheck
import Data.Text.GooglePolyline
import Control.Applicative ( (<$>) )

cRound :: Double -> Double
cRound = (/1e5) . fromIntegral . round . (*1e5)

pRound :: LatLong -> LatLong
pRound ( LatLong a b ) = LatLong (cRound a) (cRound b )

arbitraryCoord :: Gen LatLong
arbitraryCoord = do
    lat <- choose (-90.0,90.0)
    lon <- choose (-180.0,180.0)
    return $ pRound (LatLong lat lon)

instance Arbitrary LatLong where
   arbitrary = arbitraryCoord

prop_decodeInverse :: [ LatLong ] -> Bool 
prop_decodeInverse cs =
   fmap pRound ( decode . encode $ cs ) == fmap pRound  cs