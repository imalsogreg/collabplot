{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.Lazy       as T
import qualified Lucid.Svg            as L
import qualified Lucid.Svg.Elements   as E
import qualified Lucid.Svg.Attributes as A


main :: IO ()
main = L.renderToFile "test.svg" (svg (taurusWedge (TaurusWedgeSpec 0 0 20 30 0 1)))

svg :: L.Svg () -> L.Svg ()
svg content = do
  L.doctype_
  (L.svg11_ content) [L.version_ "1.1", L.width_ "300", L.height_ "300"]

data TaurusWedgeSpec = TaurusWedgeSpec {
    tsX  :: Double
  , tsY  :: Double
  , tsR0 :: Double
  , tsR1 :: Double
  , tsT0 :: Double
  , tsT1 :: Double
  } deriving (Eq, Show)

s :: (RealFrac a, Show a) => a -> String
s = show . floor

sR2 :: (RealFrac a, Show a) => (a,a) -> String
sR2 (x,y) = s x ++ " " ++ s y

taurusWedge :: TaurusWedgeSpec -> L.Svg ()
taurusWedge TaurusWedgeSpec{..} =
  let p0  = (tsR0 * cos tsT0, tsR1 * sin tsT0)
      p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
      p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
      p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
      dx0 = fst p0 - fst p3
      dy0 = snd p0 - snd p3
      dx1 = fst p2 - fst p1
      dy1 = snd p2 - snd p1
      d  = unwords ["M", s (fst p0), s (snd p0)
                   ,"L", s (fst p1), s (snd p1)
                   ,"A", s tsR1, s tsR1, "0", s dx1, s dy1
                   ,"L", s (fst p2), s (snd p2)
                   ,"A", s tsR0, s tsR0, "0", s dx0, s dy0
                   ]
      taur0 = do
        L.path_ [L.d_ (L.toHtml (T.pack d))]
  in taur0
