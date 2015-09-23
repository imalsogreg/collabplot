{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Fixed (mod')
import Data.Hash
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.Text as T
import           Lucid.Svg
import           Reflex.Dom

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

textEncode :: T.Text -> T.Text
textEncode = T.pack . show . asWord64 . hash . T.unpack


st :: String -> String
st = id

fs :: Int -> String
fs = show

fls :: Double -> String
fls = show . round

px :: Int -> String
px = (<> "px") . show

pxf :: Double -> String
pxf = px . floor

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgTag :: (MonadWidget t m) => Int -> Int -> m a -> m a
svgTag width height child =
  elC
  (defElConfig {_elConfig_namespace= Just svgNS
               ,_elConfig_attrs    = st "width" =: show width
                                     <> "height" =: show height
               })
  "svg" child

mkSvg :: ElConfig a -> ElConfig a
mkSvg e = e {_elConfig_namespace = Just svgNS}

svgEl :: MonadWidget t m => String -> m a -> m a
svgEl nm child =
  elC (defElConfig {_elConfig_namespace = Just svgNS}) nm $ child

svgElAttr:: MonadWidget t m => String -> Map String String -> m a -> m a
svgElAttr string ats child = do
  (_,c) <- svgElAttr' string ats child
  return c

svgElAttr' :: (MonadWidget t m, Attributes m attrs) => String -> attrs -> m a -> m (El t, a)
svgElAttr' string ats child = let cfg = defElConfig { _elConfig_namespace = Just svgNS
                                                    , _elConfig_attrs     = ats
                                                    }
                               in elC' cfg string child

svgElDynAttr' :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m (El t, a)
svgElDynAttr' nm dynats child =
  let cfg = defElConfig { _elConfig_namespace = Just svgNS
                        , _elConfig_attrs     = dynats}
  in elC' cfg nm child

svgElDynAttr :: MonadWidget t m => String -> Dynamic t (Map String String) -> m a -> m a
svgElDynAttr nm dynats child = do
  (_,c) <- svgElDynAttr' nm dynats child
  return c
