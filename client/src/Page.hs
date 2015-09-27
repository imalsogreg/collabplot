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
  let modelEventCommands = fmap (const) modelEvents -- TODO right, given foldr?
  --let b = modelEvents :: Int
  --let a = modelEventCommands :: Int

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

  (_,svgEvents) <- elAttr' "div" ("class" =: "main-figure") $ do

    svgEvents <- svgTag (floor svgWidth) (floor svgHeight) $ do
      bkgnd
      svgElAttr "g" ("transform" =: "translate(400 400)") $
        modelSvg model
    display model
    return svgEvents

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
