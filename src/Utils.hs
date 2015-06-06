{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Fixed (mod')
import qualified Data.Text as T
import           Lucid.Svg

gTranslate :: Double -> Double -> Svg () -> Svg ()
gTranslate dx dy content = with (g_ content) [transform_ transString]
  where transString = mconcat $ ["translate(", f dx, ",", f dy, ")"]
s :: (RealFrac a, Show a) => a -> T.Text
s = T.pack . show . floor

sR2 :: (RealFrac a, Show a) => (a,a) -> T.Text
sR2 (x,y) = mconcat [s x, " ", s y]

f :: (RealFrac a, Show a) => a -> T.Text
f = T.pack . show

fR2 :: (RealFrac a, Show a) => (a,a) -> T.Text
fR2 (x,y) = mconcat [f x, " ", f y]

type AngleRange = (Double,Double)

------------------------------------------------------------------------------
-- Utility for taking the fractional andle between beginning and end angle,
-- Accounts for the possibility that end angles straddle 0pi.
angleFrac :: AngleRange -> Double -> Double
angleFrac (th0, th1) frac
  | th1 > th0 = th0 + frac * (th1 - th0)
  | otherwise = let th1' = th1 + (2*pi)
                in  th0 + frac * (th1' - th0)

angleDiff :: AngleRange -> Double
angleDiff (th0, th1) = (th1 - th0) `mod'` (2*pi)
