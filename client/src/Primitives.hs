{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Primitives where

import Text.Read
import Control.Monad (when)
import Data.Fixed (mod')
import Data.Map (Map)
import Data.Monoid
import Lucid.Svg
import qualified Data.Text as T
import Reflex.Dom
import Reflex.Dynamic.TH
import Reflex.Dom.Contrib.Widgets.Common
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

doubleW:: MonadWidget t m => m (Dynamic t Double)
doubleW= do
  t <- _textInput_value <$> textInput def
  let mayDouble = readMaybe <$> updated t
  d <- holdDyn 0 (fmapMaybe id mayDouble)
  return d

taurusInput :: MonadWidget t m => m (Dynamic t TaurusWedgeSpec)
taurusInput = do
  x  <- doubleW
  y  <- doubleW -- _textInput_value <$> textInput def
  t  <- doubleW -- _textInput_value <$> textInput def
  dt <- doubleW -- _textInput_value <$> textInput def
  r  <- doubleW -- _textInput_value <$> textInput def
  dr <- doubleW -- _textInput_value <$> textInput def
  $(qDyn [| TaurusWedgeSpec
               ($(unqDyn [|x|])) ($(unqDyn [|y|]))  ($(unqDyn [|t|]))
                            ($(unqDyn [|dt|])) ($(unqDyn [|r|]))  ($(unqDyn [|dr|]))
         |])

------------------------------------------------------------------------------
taurusWedge :: MonadWidget t m
            => Dynamic t TaurusWedgeSpec
            -> Bool
            -> Dynamic t (Map String String)
            -> m ()
taurusWedge dynSpec b extraAttrs = do
  pathAttrs <- combineDyn (\TaurusWedgeSpec{..} extr ->
    let tsT0 = tsTheta - tsTWidth/2
        tsT1 = tsTheta + tsTWidth/2
        p0  = (tsR0 * cos tsT0, tsR0 * sin tsT0)
        p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
        p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
        p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
        largeArc = if (tsT1 - tsT0) `mod'` (2*pi) > pi then "1" else "0"
     in "d" =: mconcat
               ["M", show (fst p0 + tsX), " ", show (snd p0 + tsY), " "
               ,"L ", show (fst p1 + tsX), " ", show (snd p1 + tsY)," "
               ,unwords [ "A", show tsR1, show tsR1, "0",largeArc
                        , "1", show (fst p2 + tsX), show (snd p2 + tsY), " "]
               ,"L ", show (fst p3 + tsX), " ", show (snd p3 + tsY), " "
               ,unwords [ "A", show tsR0, show tsR0, "0", largeArc
                        , "0", show (fst p0 + tsX), show (snd p0 + tsY), " "]
               ]
          <> extr)
    dynSpec extraAttrs
  svgElDynAttr "path" pathAttrs $ return ()


textOnCircle :: MonadWidget t m
             => Dynamic t String
             -> Dynamic t (Map String String)
             -> Dynamic t Double
             -> Dynamic t Double
             -> Dynamic t (Maybe Double)
             -> m ()
textOnCircle txt extraAttrs dynRad dynTheta dynMayRayLength = do

  dynPathName <- combineDyn
                 (\r t -> mconcat ["Radius", show r ,"Theta", show t])
                 dynTheta dynRad

  pathURL <- forDyn dynPathName $ \p -> '#':p

  svgElAttr "g" ("class" =: "text-on-path") $ do
    svgEl "defs" $ do
      pathAttrs <- $(qDyn [| "id" =: $(unqDyn [| dynPathName |])
                          <> "d"  =: d $(unqDyn [|dynRad|])
                                       $(unqDyn [|dynTheta|])
                                       $(unqDyn [|dynMayRayLength|]) |])
      svgElDynAttr "path" pathAttrs $ return ()

    useAttrs <- forDyn pathURL $ \u -> (Just xlinkNS, "xlink:href") =: u
                                    <> (Nothing,      "fill")       =: "none"
    svgElDynAttrNS' "use" useAttrs $ return ()

    let textAttrs = -- "font-family" =: "Verdana"
                 -- <> "font-size"   =: "12pt"
                 -- <>
                 "text-anchor" =: "middle"
    svgElAttr "text" textAttrs $ do

      textPathAttrs <- forDyn pathURL $ \u ->
         (Just xlinkNS, "xlink:href")      =: u
         <> (Nothing, "startOffset")       =: "50%"
         <> (Nothing, "dominant-baseline") =: "middle"
      svgElDynAttrNS' "textPath" textPathAttrs $ dynText txt
      return ()

    where
      d radius theta rayLen = case rayLen of
           -- When no ray length is given, put text on horizontal path
           Nothing -> mconcat ["M",  show (radius * cos (theta - pi/3)), " "
                              ,      show (radius * sin (theta - pi/3)), " "
                              ,"A ", show radius, " ", show radius, " 0 0 1 "
                              , show (radius * cos (theta + pi/3)), " "
                              , show (radius * sin (theta + pi/3))
                              ]
           -- When ray length is given, put text on vertical path
           Just r2 -> mconcat ["M", show ((radius - r2/2) * cos theta), " "
                              ,     show ((radius - r2/2) * sin theta), " "
                              ,"L ", show ((radius + r2/2) * cos theta), " "
                              ,      show ((radius + r2/2) * sin theta)]


highLine :: MonadWidget t m
         => Dynamic t (Double, Double)
         -> Dynamic t Double
         -> Dynamic t Double
         -> Dynamic t (Map String String)
         -> m (Event t ())
highLine dynRng dynRad dynHeight extraAttrs = do
  pathAttrs <- $(qDyn
    [| let (th0,th1) = $(unqDyn [| dynRng |])
           r         = $(unqDyn [| dynRad |])
           h         = $(unqDyn [| dynHeight |])  :: Double
           (x0,y0)   = (r * cos th0, r * sin th0)
           (x1,y1)   = (r * cos th1, r * sin th1)
           dx        = x1 - x0
           dy        = y1 - y0
           extras    = $(unqDyn [| extraAttrs |])
           slopeX    = const 0 :: Double -> Double
           slopeY th = (-1) * h * abs (circularDist th0 th1 / 2) :: Double
           d = unwords [ "M" <> show x0, show y0
                       , "c", show (slopeX th0), show (slopeY th0) <> ","
                       , show (slopeX th1 + dx), show (slopeY th1 + dy) <> ","
                       , show (dx),              show (dy)
                       ]
         in "d" =: d <> extras
    |])
  svgElDynAttr "path" pathAttrs $ return ()
  return never
