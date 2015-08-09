{-| 

Provides encoding and decoding of Google Polyline text which is used
in the Google Maps API.  A description of the polyline format and
algorithm can be found at

<https://developers.google.com/maps/documentation/utilities/polylinealgorithm>.

-}

module Data.Text.GooglePolyline ( LatLong(..), encode, decode )
       where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char as C
import Data.Bits 
import Data.Int ( Int32 )

-- | Type to encapsulate latitude/longitude pairs.
data LatLong = LatLong
  { latitude :: Double
  , longitude :: Double
  } deriving ( Show, Eq )

toInts :: BSC.ByteString -> [ Int32 ]
toInts = map fromIntegral . BS.unpack

-- Step 11
toChrs :: [ Int32 ] -> T.Text
toChrs = T.pack . map ( C.chr . fromIntegral )

sub63 :: [ Int32 ] -> [ Int32 ]
sub63 = map ( (-63) + )

-- Step 10
add63 :: [ Int32 ] -> [ Int32 ]
add63 = map ( 63 + )

splitChunk :: [ Int32 ] -> ( [ Int32 ], [ Int32 ] )
splitChunk is = let (a,b) = span ( >= 0x20 ) is in
  (a ++ [head b], tail b )

-- Step 8 and 9  
delimChunk :: [ Int32 ] -> [ Int32 ]
delimChunk is = map ( .|. 0x20) ( init is )  ++ [ last is ]

dechunk :: [ Int32 ] -> Int32
dechunk is = let withShifts = zip is [0,5..]
             in
              sum $ map ( \(c,s) -> shift (c .&. 0x1F) s ) withShifts

-- Step 6 and 7
enchunk :: Int32 -> [ Int32 ]
enchunk 0 = [ 0 ] -- Special case for 0.  Code below would result in an empty.
enchunk i =
   let shifts = [ 0,5.. ]
       shifted = takeWhile (>0) $ map ( \s -> shift i (-s) ) shifts
   in
   map ( .&. 0x1F ) shifted

-- Inverse of negEnc             
negDec :: Int32 -> Int32 
negDec i = let x = shift i (-1) in
  if testBit i 0 then complement x else x

-- Google steps 3 to 5                                          
negEnc :: Int32 -> Int32
negEnc i = let x = shift i 1 in
  if i < 0 then complement x else x

toDouble :: Int32 -> Double
toDouble x = fromIntegral x / 1e5

fromDouble :: Double -> Int32
fromDouble x = round ( x * 1e5 )

oneCoordOp :: [Int32] -> Double
oneCoordOp = toDouble . negDec . dechunk

oneCoordEnc :: Double -> T.Text
oneCoordEnc = toChrs . add63 . delimChunk . enchunk . negEnc . fromDouble

stringPrep :: BSC.ByteString -> [ Int32 ]
stringPrep = sub63 . toInts

toChunks :: [ Int32 ] -> [ [Int32] ]
toChunks [] = []
toChunks xs = let (c,cs) = splitChunk xs in
  c : toChunks cs
  
stringToCoords :: BSC.ByteString -> [ Double ]
stringToCoords  = map oneCoordOp . toChunks . stringPrep

makePairs :: [ Double ] -> [ LatLong ]
makePairs [] = []
makePairs (d1:d2:ds) = LatLong d1 d2 : makePairs ds

catPairs :: [ LatLong ] -> [ Double ]
catPairs [] = []
catPairs ( LatLong a b : xs ) = a : b : catPairs xs

addPair :: LatLong -> LatLong -> LatLong
addPair (LatLong x1 y1) (LatLong x2 y2) = LatLong (x1 + x2) (y1 + y2)

subPair :: LatLong -> LatLong -> LatLong
subPair (LatLong x1 y1) (LatLong x2 y2) = LatLong (x1 - x2) (y1 - y2)

-- inverse of ( scanl1 addPair )
adjDiff :: [ LatLong ] -> [ LatLong ]
adjDiff p = zipWith subPair p (LatLong 0 0:p)

-- | Decodes Google Polyline text to a sequence of Latitude/Longitude
-- points.  Inverse of 'encode'.
decode :: T.Text -> [ LatLong ]
decode = scanl1 addPair . makePairs . stringToCoords . TE.encodeUtf8

-- | Encodes a sequence of Latitude/Longitude points into Google
-- Polyline text.  Inverse of 'decode'.
encode :: [ LatLong ] -> T.Text
encode = T.concat . fmap oneCoordEnc . catPairs . adjDiff