{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Primitives where

import Control.Monad (when)
import Data.Fixed (mod')
import Data.Monoid
import Lucid.Svg
import qualified Data.Text as T
import Utils


------------------------------------------------------------------------------
data TaurusWedgeSpec = TaurusWedgeSpec {
    tsX      :: Double
  , tsY      :: Double
  , tsR0     :: Double
  , tsR1     :: Double
  , tsTheta  :: Double
  , tsTWidth :: Double
  } deriving (Eq, Show)


------------------------------------------------------------------------------
circularDist :: Double -> Double -> Double
circularDist x1 x0 = (x1 - x0 + pi `mod'` (2*pi)) - pi


------------------------------------------------------------------------------
taurusWedge :: TaurusWedgeSpec -> Bool -> Svg ()
taurusWedge TaurusWedgeSpec{..} extraNub =
  let tsT0 = tsTheta - tsTWidth/2
      tsT1 = tsTheta + tsTWidth/2
      p0  = (tsR0 * cos tsT0, tsR0 * sin tsT0)
      p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
      p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
      p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
      largeArc = if (tsT1 - tsT0) `mod'` (2*pi) > pi then "1" else "0"
      d  = mconcat ["M", f (fst p0), " ", f (snd p0), " "
                   ,"L ", f (fst p1), " ", f (snd p1)," "
                   ,T.unwords [ "A", f tsR1, f tsR1, "0",largeArc
                              , "1", f (fst p2), f (snd p2), " "]
                   ,"L ", f (fst p3), " ", f (snd p3)
                   ,T.unwords [ "A", f tsR0, f tsR0, "0", largeArc
                              , "0", f (fst p0), f (snd p0)]
                   ] :: T.Text
      taur0 = g_ $ do
        path_ [d_ d]
        when extraNub $ circle_ [ cx_ (f (tsR1 * cos tsTheta))
                                , cy_ (f (tsR1 * sin tsTheta))
                                , r_  "10"
                                ]
  in taur0


------------------------------------------------------------------------------
textOnCircle :: T.Text -> [Attribute] -> Double -> Double -> Maybe Double -> Svg ()
textOnCircle txt txtAttributes radius theta rayLength = g_ $ do
  defs_ $ do
    path_ [d_ d, id_ pathName]
  use_ [xlinkHref_ pathUrl, fill_ "none"]
  text_ [font_family_ "Verdana", font_size_ "20", text_anchor_ "middle"] $
    with ((term "textPath" ) (toHtml txt))
    [xlinkHref_ pathUrl
    , startOffset_ "50%"
    , dominant_baseline_ "middle"
    ]
    where
      d = case rayLength of
           -- When no ray length is given, put text on horizontal path
           Nothing -> mconcat ["M",  f (radius * cos (theta - pi/3)), " "
                              ,      f (radius * sin (theta - pi/3)), " "
                              ,"A ", f radius, " ", f radius, " 0 0 1 "
                              , f (radius * cos (theta + pi/3)), " "
                              , f (radius * sin (theta + pi/3))
                              ] :: T.Text
           -- When ray length is given, put text on vertical path
           Just r2 -> mconcat ["M", f ((radius - r2/2) * cos theta), " "
                              ,     f ((radius - r2/2) * sin theta), " "
                              ,"L ", f ((radius + r2/2) * cos theta), " "
                              ,      f ((radius + r2/2) * sin theta)]
      pathName = mconcat ["Radius", f radius, "Theta", f theta]
      pathUrl  = "#" <> pathName


------------------------------------------------------------------------------
highLine :: Double -> Double -> Double -> Double -> Svg ()
highLine th0 th1 radius height = path_ [d_ d]
  where (x0,y0)   = (radius * cos th0, radius * sin th0)
        (x1,y1)   = (radius * cos th1, radius * sin th1)
        dx        = x1 - x0
        dy        = y1 - y0
        slopeX th = 0 -- height * cos th / 2
        slopeY _  = (-1) * height * abs (circularDist th0 th1 / 2)
        d = T.unwords ["M" <> f x0, f y0
                      ,"c", f (slopeX th0), f (slopeY th0) <> ","
                      ,     f (slopeX th1 + dx), f (slopeY th1 + dy) <> ","
                      ,     f (dx),         f (dy)
                      ]
