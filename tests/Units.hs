{-# LANGUAGE OverloadedStrings #-}
module Units where

import Test.HUnit

import Data.Text.GooglePolyline

import qualified Data.Text as T

maxDiff :: [ LatLong ] -> [ LatLong ] -> Double
maxDiff p q = maximum $
   zipWith 
       ( \(LatLong x y) (LatLong a b) -> 
           sqrt ( ( a - x )^2 + ( b - y )^2 ) ) p q

assertPathEqual :: String -> [ LatLong ] -> [ LatLong ] -> Assertion
assertPathEqual s p1 p2 = assertBool s ( maxDiff p1 p2 < 1e-10 )

datNWenc :: T.Text
datNWenc = "i}q~FrdzuO"

datNWdec :: [ LatLong ]
datNWdec = [LatLong 41.87621 (-87.6297)]

singlePointNWenc :: Assertion
singlePointNWenc =
    assertEqual "Encode Single Point NW" datNWenc ( encode datNWdec )

singlePointNWdec :: Assertion
singlePointNWdec =
    assertEqual "Decode Single Point NW" datNWdec ( decode datNWenc )

datNEenc :: T.Text
datNEenc = "gqp|F}vjuA"

datNEdec :: [ LatLong ]
datNEdec = [LatLong 41.54148 14.15039]

singlePointNEenc :: Assertion
singlePointNEenc =
    assertEqual "Encode Single Point NE" datNEenc ( encode datNEdec )

singlePointNEdec :: Assertion
singlePointNEdec = 
    assertEqual "Decode Single Point NE" datNEdec ( decode datNEenc )

datSWenc :: T.Text
datSWenc = "x`hmEha|oL"

datSWdec :: [ LatLong ]
datSWdec = [LatLong (-33.79741) (-70.92773)]

singlePointSWenc :: Assertion
singlePointSWenc =
    assertEqual "Encode Single Point SW" datSWenc ( encode datSWdec )

singlePointSWdec :: Assertion
singlePointSWdec =
    assertEqual "Decode Single Point SW" datSWdec ( decode datSWenc )

datSEenc :: T.Text
datSEenc = "`zfuE__~lY"

datSEdec :: [ LatLong ]
datSEdec = [LatLong (-35.10193) 138.60352]

singlePointSEenc :: Assertion
singlePointSEenc =
    assertEqual "Encode Single Point SE" datSEenc ( encode datSEdec )

singlePointSEdec :: Assertion
singlePointSEdec =
    assertEqual "Decode Single Point SE" datSEdec ( decode datSEenc )

singlePointZeroEnc :: Assertion
singlePointZeroEnc =
    assertEqual "Encode zero point (0,0)" "??" ( encode $ [LatLong 0 0] )

singlePointZeroDec :: Assertion
singlePointZeroDec =
    assertEqual "Encode zero point (0,0)" [LatLong 0 0] ( decode "??" )

dat3PathEnc :: T.Text
dat3PathEnc = "iftwFdlsbM~_Ak_AxiAppA"

dat3PathDec :: [ LatLong ]
dat3PathDec = [LatLong 40.74101 (-73.99635)
              ,LatLong 40.73061 (-73.98605)
              ,LatLong 40.71864 (-73.99910)]

threePointPathEnc :: Assertion
threePointPathEnc =
   assertEqual "Encode Three point path" dat3PathEnc ( encode dat3PathDec )

threePointPathDec :: Assertion
threePointPathDec =
   assertPathEqual "Decode Three point path" dat3PathDec ( decode dat3PathEnc )