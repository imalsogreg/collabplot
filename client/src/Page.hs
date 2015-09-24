{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE RankNTypes        #-}

module Page where

import qualified Data.Aeson as A
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import Data.Default
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Lucid
import qualified Lucid as L
import qualified Lucid.Svg as LSvg
import Lucid.Svg (Svg(..), renderText)
import CollabTypes
import Utils
import Network.HTTP.Base (urlEncode, urlDecode)
import Figure
import Reflex
import Primitives
import Shadow
import Menus
import Reflex.Dom

pageWidget :: MonadWidget t m => m ()
pageWidget = mdo

  pb <- getPostBuild
  let  modelUrls = "/model" <$ (leftmost [pb
                                         , () <$ piUpdates
                                         , () <$ memberUpdates
                                         , () <$ projectUpdates
                                         ])
  modelEvents <- fmapMaybe id <$> getAndDecode modelUrls
  model       <- holdDyn (Model [] []) modelEvents

  piUpdates <- newPIBox model
  el "br" (return ())
  memberUpdates <- newMemberBox model
  el "br"  (return ())
  projectUpdates <- newProjectBox model

  -- menuEvents <- menusWidget pictureEvents
  -- infoWidget menuEvents

  (fig,_) <- elAttr' "div" ("class" =: "main-figure") $ do
    --svgDyn =<< (mapDyn modelSvg model)
    --pictureEvents <- pictureWidget menuEvents
    tws <- taurusInput
    svgTag (floor svgWidth) (floor svgHeight) $ do
      bkgnd'
      svgElAttr "g" ("transform" =: "translate(200 200)") $ modelSvg' model
      --
      -- piWedges' model
      -- thrustWedges' model
      -- -- cicr <- elShadow' defShadowParams $ svgElAttr "circle" ("cx" =: "10" <> "cy" =: "10" <> "r" =: "50") $ return ()
      -- --taurusWedge' (constDyn (TaurusWedgeSpec 50 50 20 10 0 1)) False (constDyn ("fill" =: "red"))
      -- elShadow' defShadowParams $ taurusWedge' tws' False (constDyn ("fill" =: "red"))
      -- elShadow' defShadowParams $ taurusWedge' tws'' False (constDyn ("fill" =: "red"))
      -- elShadow' defShadowParams $ taurusWedge' tws''' False (constDyn ("fill" =: "red"))
      return ()
    display model

  let mEvents = domEvent Mousemove fig
  evt <- holdDyn (0,0) mEvents
  tws' <- forDyn evt $ \(x,y) ->
    TaurusWedgeSpec 200 200 20 50
      (fromIntegral x * 2 * pi / 400)
      (fromIntegral y * 2 * pi / 400)
  tws'' <- forDyn evt $ \(x,y) ->
    TaurusWedgeSpec 200 200 60 90
      (fromIntegral x * 2 * pi / 200)
      (fromIntegral y * 2 * pi / 100)
  tws''' <- forDyn evt $ \(x,y) ->
    TaurusWedgeSpec 200 200 100 140
      (fromIntegral x * 2 * pi / 400)
      (fromIntegral y * 2 * pi / 200)

  return ()

-- LUCID
svgDyn :: (MonadWidget t m) => Dynamic t (Svg ()) -> m ()
svgDyn figDyn = do
  svgDyn <- mapDyn (svg . (bkgnd <>)) figDyn
  elDynHtml' "div" =<< mapDyn (TL.unpack . renderText) svgDyn
  return ()


svgHeight, svgWidth :: Double
svgHeight = 800
svgWidth  = 800

bkgnd :: LSvg.Svg ()
bkgnd = do
  LSvg.defs_ $ do
    LSvg.radialGradient_ [LSvg.id_ "bkgndGradient"
                         , LSvg.cx_ "0.6"
                         , LSvg.cy_ "0.6"
                         , LSvg.r_  "0.4"] $ do
      LSvg.stop_ [LSvg.offset_ "0%", LSvg.stop_color_ "#1b5354"]
      LSvg.stop_ [LSvg.offset_ "100%", LSvg.stop_color_ "#0f2d2d"]
  LSvg.rect_ [LSvg.x_ (f (svgWidth/(-2))), LSvg.y_ (f (svgHeight/(-2)))
             , LSvg.width_ (f svgWidth), LSvg.height_ (f svgHeight)
             , LSvg.fill_ "url(#bkgndGradient)"]

bkgnd' :: MonadWidget t m => m ()
bkgnd' = do

  svgEl "defs" $ do
    svgElAttr "radialGradient" ("id" =: st "bkgndGradient"
                       <> "cx" =: "0.6" <> "cy" =: "0.6"
                       <> "r" =: "0.4") $ do
        svgElAttr "stop" ("offset" =: st "0%"
                       <> "stop-color" =: "#1b5354") $ return ()
        svgElAttr "stop" ("offset" =: st "100%"
                       <> "stop-color" =: "#0f2d2d") $ return ()

  svgElAttr "rect" ("x" =: pxf (svgWidth / (-2))
                 <> "y" =: pxf (svgHeight / (-2))
                 <> "width" =: pxf svgWidth
                 <> "height" =: pxf svgHeight
                 <> "fill" =: "url(#bkgndGradient)") $ return ()

svg :: LSvg.Svg () -> LSvg.Svg ()
svg content = do
  LSvg.doctype_
  LSvg.with (LSvg.svg11_ (gTranslate 400 400 content))
    [LSvg.version_ "1.1", LSvg.width_ "800", LSvg.height_ "800"]





-- ------------------------------------------------------------------------------
-- page :: FilePath -> Model -> Svg () -> Html ()
-- page jsPath Model{..} figSvg = html_ $ do  -- Todo drop filepath thing
--   doctype_
--   head_ $ do
--     link_ [rel_ "stylesheet", type_ "text/css", href_ "default.css"]
--     --script_ [src_ (T.pack jsPath)]
--     (termWith "script" [src_ "http://code.jquery.com/jquery-2.1.4.min.js"] (return ()))
--   body_ $ do
--     div_ [class_ "main-figure"] $ figSvg
--     forM_ projects $ \p -> do
--       div_ [class_ "synopsis"
--            ,id_    (textEncode $ projectName p)] $ do
--         table_ $ do
--           tr_ $ td_ [class_ "field"] "Project name:" <> td_ [class_ "val"] (toHtml $ projectName p)
--           tr_ $ td_ [class_ "field"] "Members:"      <> td_ [class_ "val"] (foldMap toHtml $ projectMembers p)
--     (termWith "script" [src_ (T.pack "collab.js")] (return ()))
