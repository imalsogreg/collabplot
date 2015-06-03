{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Fixed           (mod')
import           Data.Monoid
import qualified Data.Text            as T
import           Lucid.Svg
import qualified Lucid.Svg            as L
import qualified Lucid.Svg.Elements   as E
import qualified Lucid.Svg.Attributes as A
import           Shadow

main :: IO ()
main = L.renderToFile "test.svg" (svg . (textOnWedge "Hella" [] 100 0 <>) . mconcat $ flip map [-4,-3.5 .. 4] $ \x ->
                                     mconcat (flip map [-4, -3.5 .. 4] $ \y ->
                                       gTranslate (150 * x) (150 * y) (testWedge x y)))

testWedge :: Double -> Double -> Svg ()
testWedge a0 a1 = taurusWedge (TaurusWedgeSpec 0 0 15 35 a0 a1)

svg :: L.Svg () -> L.Svg ()
svg content = do
  L.doctype_
  L.with (L.svg11_ (gTranslate 400 400 content)) [L.version_ "1.1", L.width_ "900", L.height_ "900"]

gTranslate :: Double -> Double -> Svg () -> Svg ()
gTranslate dx dy content = with (g_ content) [transform_ transString]
  where transString = mconcat $ ["translate(", f dx, ",", f dy, ")"]

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

sR2 :: (RealFrac a, Show a) => (a,a) -> T.Text
sR2 (x,y) = mconcat [s x, " ", s y]

f :: (RealFrac a, Show a) => a -> T.Text
f = T.pack . show

fR2 :: (RealFrac a, Show a) => (a,a) -> T.Text
fR2 (x,y) = mconcat [f x, " ", f y]

circularDist :: Double -> Double -> Double
circularDist x1 x0 = (x1 - x0 + pi `mod'` (2*pi)) - pi

taurusWedge :: TaurusWedgeSpec -> L.Svg ()
taurusWedge TaurusWedgeSpec{..} =
  let p0  = (tsR0 * cos tsT0, tsR0 * sin tsT0)
      p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
      p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
      p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
      largeArc = if (tsT1 - tsT0) `mod'` (2*pi) > pi then "1" else "0"
      d  = mconcat ["M", f (fst p0), " ", f (snd p0), " "
                   ,"L ", f (fst p1), " ", f (snd p1)," "
                   ,"A ", f tsR1, " ", f tsR1, " 0 ", largeArc, " 1 ", f (fst p2), " ", f (snd p2), " "
                   ,"L ", f (fst p3), " ", f (snd p3)
                   ,"A ", f tsR0, " ", f tsR0, " 0 ", largeArc, " 0 ", f (fst p0), " ", f (snd p0)
                   ] :: T.Text
      taur0 = do
        L.path_ [L.d_ d]
  in taur0

textOnWedge :: T.Text -> [Attribute] -> Double -> Double -> Svg ()
textOnWedge txt txtAttributes radius theta = g_ $ do
  defs_ $ do
    path_ [d_ d, id_ pathName]
  use_ [xlinkHref_ pathUrl, fill_ "none", stroke_ "red"]
  text_ [font_family_ "Verdana", font_size_ "30"] $
    with ((term "textPath" ) (toHtml txt)) [xlinkHref_ pathUrl]

    where
      d        = mconcat ["M",  f (radius * cos (theta - pi/3)), " "
                         ,      f (radius * sin (theta - pi/3)), " "
                         ,"A ", f radius, f radius, " 1 0 0 "
                         , f (radius * cos (theta + pi/2)), " "
                         , f (radius * sin (theta + pi/2))
                         ] :: T.Text
      pathName = mconcat ["Radius", f radius, "Theta", f theta]
      pathUrl  = "#" <> pathName
