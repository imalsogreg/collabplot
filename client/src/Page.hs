{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE RankNTypes        #-}

module Page where

import qualified Data.Aeson as A
import Data.Bool
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import Data.Default
import Data.Foldable
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Time
--import Lucid
--import qualified Lucid as L
--import qualified Lucid.Svg as LSvg
--import Lucid.Svg (Svg(..), renderText)
import CollabTypes
import Utils
import Network.HTTP.Base (urlEncode, urlDecode)
import Figure
import Reflex
import Primitives
import Shadow
import Menus
import Reflex.Dom

pageWidget :: MonadWidget t m => UTCTime -> m ()
pageWidget t0 = mdo

  pb <- getPostBuild
  let  modelUrls = "/model" <$ (leftmost [pb
                                         , () <$ piUpdates
                                         , () <$ memberUpdates
                                         , () <$ projectUpdates
                                         ])

  tickTimes <- fmap _tickInfo_lastUTC <$> tickLossy 0.1 t0
  modelEvents <- fmapMaybe id <$> getAndDecode modelUrls


  focusEvents    <- fmap (fmapMaybe id) $ fmap (updated . nubDyn) $
                    mapDyn _modelFocus model
  let focusModels     = tag (current model) focusEvents
      focusModelsOpts = attach (current dynFigOpts) focusModels

  dynFocusPI     <- mapDyn _modelFocus model
  let figOptsAtFocus = attach (current dynFigOpts) focusEvents
  let angEvents  = ffor focusModelsOpts $ \(figOpts, m) ->
                     (_modelFocus m >>= flip Map.lookup (piAngles m figOpts))


  focusTimesAngles <- performEvent (fmap (\a -> do
                                            t <- liftIO getCurrentTime
                                            return (t,a)) (fmapMaybe id angEvents))

  let aux (t,a) p = MotionPlan t 5 (rEnd p) a
      plan0 = MotionPlan (UTCTime (fromGregorian 2015 1 1) 0) 0 0 0
  dynMotionPlan <- foldDyn aux plan0 focusTimesAngles

  let modelEventCommands = fmap (const) modelEvents -- TODO right, given foldr?

  lastMouseMove <- holdDyn (0,0) moves

  let timedPlans   = attach (current dynMotionPlan) tickTimes
      figAngEvents    = (fmapMaybe id) $ ffor timedPlans (uncurry (flip runMotionPlan))
      figOptEvents = ffor figAngEvents $ \ang -> defFigOpts {thrustThetaOffset = ang}

  dynFigOpts <- holdDyn defFigOpts figOptEvents
  --dynFigOpts <- holdDyn defFigOpts (ffor tickTimes $ \t -> defFigOpts {thrustThetaOffset = realToFrac (diffUTCTime t t0)})
  display dynMotionPlan
  display dynFigOpts

  -- dynFigOpts <- forDyn lastMouseMove $ \(x,y) ->
  --   defFigOpts { thrustRadiusMin = 255
  --              , thrustRadiusMax = fromIntegral y
  --              , thrustThetaOffset = fromIntegral x / 100}
  --dynMotionPlan <- foldDyn ($) Nothing (leftmost [])
  --dynFigOpts <- foldDyn (\)

  model <- foldDyn id -- (flip (foldr ($)))
                   (Model [] [] [] Nothing)
                   (leftmost [modelEventCommands, svgEvents])

  piUpdates <- visToggleBox (text "PI") (newPIBox model)
  el "br" (return ())
  memberUpdates <- visToggleBox (text "M") (newMemberBox model)
  el "br"  (return ())
  projectUpdates <- visToggleBox (text "Proj") (newProjectBox model)

  -- menuEvents <- menusWidget pictureEvents
  -- infoWidget menuEvents

  (fig,svgEvents) <- elAttr' "div" ("class" =: "main-figure") $ do

    svgEvents <- svgTag (floor svgWidth) (floor svgHeight) $ do
      bkgnd
      svgElAttr "g" ("transform" =: "translate(400 400)") $
        modelSvg model dynFigOpts
    --display model
    return svgEvents

  let moves = domEvent Mousemove fig

  return ()


svgHeight, svgWidth :: Double
svgHeight = 800
svgWidth  = 800

bkgnd :: MonadWidget t m => m ()
bkgnd = do

  svgEl "defs" $ do
    svgElAttr "radialGradient" ("id" =: st "bkgndGradient"
                       <> "cx" =: "0.6" <> "cy" =: "0.6"
                       <> "r" =: "0.4") $ do
        svgElAttr "stop" ("offset" =: st "0%"
                       <> "stop-color" =: "#1b5354") $ return ()
        svgElAttr "stop" ("offset" =: st "100%"
                       <> "stop-color" =: "#0f2d2d") $ return ()

  svgElAttr "rect" ("x" =: "0" -- pxf (svgWidth / (-2))
                 <> "y" =: "0" -- pxf (svgHeight / (-2))
                 <> "width" =: pxf svgWidth
                 <> "height" =: pxf svgHeight
                 <> "fill" =: "url(#bkgndGradient)") $ return ()

data MotionPlan = MotionPlan
  { tStart :: UTCTime
  , duration :: Double
  , rStart   :: Double
  , rEnd     :: Double
  } deriving (Show)

runMotionPlan :: UTCTime -> MotionPlan -> Maybe Double
runMotionPlan t mp =
      let x  = realToFrac (diffUTCTime t (tStart mp))
          y  = rStart mp + (rStart mp - rEnd mp) / (duration mp) * (x)
      in  bool Nothing (Just y) (x < duration mp)
