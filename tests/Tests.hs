import Units
import QuickCheck

import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Unit Tests" [
      testCase "North West Point-Enc" singlePointNWenc
    , testCase "North East Point-Enc" singlePointNEenc
    , testCase "South West Point-Enc" singlePointSWenc
    , testCase "South East Point-Enc" singlePointSEenc
    , testCase "Zero Point-Enc" singlePointZeroEnc
    , testCase "Three Point Path-Enc" threePointPathEnc
    , testCase "North West Point-Dec" singlePointNWdec
    , testCase "North East Point-Dec" singlePointNEdec
    , testCase "South West Point-Dec" singlePointSWdec
    , testCase "South East Point-Dec" singlePointSEdec
    , testCase "Zero Point-Dec" singlePointZeroDec
    , testCase "Three Point Path-Dec" threePointPathDec
    ]
    ,
    testGroup "QuickCheck" [ 
       testProperty "Decode as inverse" prop_decodeInverse
    ]
  ]