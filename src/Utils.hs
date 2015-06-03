{-# LANGUAGE OverloadedStrings #-}

module Utils where

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
