{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Fixed           (mod')
import qualified Data.Text            as T
import           Lucid.Svg
import qualified Lucid.Svg            as L
import qualified Lucid.Svg.Elements   as E
import qualified Lucid.Svg.Attributes as A
import           Shadow

main :: IO ()
main = L.renderToFile "test.svg" (svg . mconcat $ flip map [0] $ \x ->
                                     mconcat (flip map [0 .. 6] $ \y ->
                                       gTranslate (70 * y) (70 * y) (testWedge x y)))

testWedge :: Double -> Double -> Svg ()
testWedge a0 a1 = taurusWedge (TaurusWedgeSpec 0 0 7 25 a0 a1)

svg :: L.Svg () -> L.Svg ()
svg content = do
  L.doctype_
  L.with (L.svg11_ (gTranslate 400 400 content)) [L.version_ "1.1", L.width_ "900", L.height_ "900"]

gTranslate :: Double -> Double -> Svg () -> Svg ()
gTranslate dx dy content = with (g_ content) [transform_ transString]
  where transString = mconcat $ ["translate(", s dx, ",", s dy, ")"]

data TaurusWedgeSpec = TaurusWedgeSpec {
    tsX  :: Double
  , tsY  :: Double
  , tsR0 :: Double
  , tsR1 :: Double
  , tsT0 :: Double
  , tsT1 :: Double
  } deriving (Eq, Show)

s :: (RealFrac a, Show a) => a -> T.Text
s = T.pack . show . floor

f :: (RealFrac a, Show a) => a -> T.Text
f = T.pack . show

sR2 :: (RealFrac a, Show a) => (a,a) -> T.Text
sR2 (x,y) = mconcat [s x, " ", s y]

taurusWedge :: TaurusWedgeSpec -> L.Svg ()
taurusWedge TaurusWedgeSpec{..} =
  let p0  = (tsR0 * cos tsT0, tsR1 * sin tsT0)
      p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
      p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
      p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
      largeArc = if (tsT1 - tsT0) `mod'` (2*pi) < pi then "0" else "1"
      d  = mconcat ["M", s (fst p0), " ", s (snd p0), " "
                   ,"L ", s (fst p1), " ", s (snd p1)," "
                   ,"A ", s tsR1, " ", s tsR1, " 0 ", largeArc, " 1 ", s (fst p2), " ", s (snd p2), " "
                   ,"L ", s (fst p3), " ", s (snd p3)
                   ,"A ", s tsR0, " ", s tsR0, " 0 ", largeArc, " 0 ", s (fst p0), " ", s (snd p0)
                   ] :: T.Text
      taur0 = do
        L.path_ [L.d_ d]
        circle_ [cx_ (s (fst p0)), cy_ (s (snd p0)), r_ "3", fill_ "red"]
  in taur0
