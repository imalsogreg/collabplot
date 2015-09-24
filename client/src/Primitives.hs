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
taurusWedge' :: MonadWidget t m
             => Dynamic t TaurusWedgeSpec
             -> Bool
             -> Dynamic t (Map String String)
             -> m ()
taurusWedge' dynSpec b extraAttrs = do
  pathAttrs <- combineDyn (\TaurusWedgeSpec{..} extr ->
    let tsT0 = tsTheta - tsTWidth/2
        tsT1 = tsTheta + tsTWidth/2
        p0  = (tsR0 * cos tsT0, tsR0 * sin tsT0)
        p1  = (tsR1 * cos tsT0, tsR1 * sin tsT0)
        p2  = (tsR1 * cos tsT1, tsR1 * sin tsT1)
        p3  = (tsR0 * cos tsT1, tsR0 * sin tsT1)
        largeArc = if (tsT1 - tsT0) `mod'` (2*pi) > pi then "1" else "0"
     in "d" =: mconcat ["M", show (fst p0 + tsX), " ", show (snd p0 + tsY), " "
                       ,"L ", show (fst p1 + tsX), " ", show (snd p1 + tsY)," "
                       ,unwords [ "A", show tsR1, show tsR1, "0",largeArc
                                , "1", show (fst p2 + tsX), show (snd p2 + tsY), " "]
                       ,"L ", show (fst p3 + tsX), " ", show (snd p3 + tsY)
                       ,unwords [ "A", show tsR0, show tsR0, "0", largeArc
                                , "0", show (fst p0 + tsX), show (snd p0 + tsY)]
                       ]
          <> extr)
    dynSpec extraAttrs
  svgElDynAttr "path" pathAttrs $ return ()

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

textOnCircle' :: MonadWidget t m
              => Dynamic t String
              -> Dynamic t (Map String String)
              -> Dynamic t Double
              -> Dynamic t Double
              -> Dynamic t (Maybe Double)
              -> m ()
textOnCircle' txt extraAttrs dynRad dynTheta dynMayRayLength = do

  dynPathName <- combineDyn
                 (\r t -> mconcat ["Radius", show r ,"Theta", show t])
                 dynTheta dynRad

  pathURL <- forDyn dynPathName $ \p -> '#':p

  svgEl "defs" $ do
    pathAttrs <- $(qDyn [| "id" =: $(unqDyn [| dynPathName |])
                        <> "d"  =: d $(unqDyn [|dynRad|])
                                     $(unqDyn [|dynTheta|])
                                     $(unqDyn [|dynMayRayLength|]) |])
    svgElDynAttr "path" pathAttrs $ return ()

  useAttrs <- forDyn pathURL $ \u -> "xlinkHref" =: u <> "fill" =: "none"
  svgElDynAttr "use" useAttrs $ return ()

  let textAttrs = "font-family" =: "Verdana"
               <> "font-size"   =: "20pt"
               <> "text-anchor" =: "middle"
  svgElAttr "text" textAttrs $ do

    textPathAttrs <- forDyn pathURL $ \u -> "xlinkHref"         =: u
                                         <> "startOffset"       =: "50%"
                                         <> "dominant-baseline" =: "middle"
    svgElDynAttr "textPath" textPathAttrs $ dynText txt

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

highLine' :: MonadWidget t m
          => Dynamic t (Double, Double)
          -> Dynamic t Double
          -> Dynamic t Double
          -> Dynamic t (Map String String)
          -> m (Event t ())
highLine' dynRng dynRad dynHeight extraAttrs = do
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
           d = mconcat [ "M" <> show x0, show y0
                       , "c", show (slopeX th0), show (slopeY th0) <> ","
                       , show (slopeX th1 + dx), show (slopeY th1 + dy) <> ","
                       , show (dx),              show (dy)
                       ]
         in "d" =: d <> extras
    |])
  svgElDynAttr "path" pathAttrs $ return ()
  return never
