{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.Fixed                 (mod')
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Traversable
import           Figure
import qualified Language.Javascript.JQuery as JQuery
import           Lucid                      as L
import qualified Lucid.Svg                  as LSvg
import           Model
import           Page
import           Primitives
import           Shadow
import           Utils


------------------------------------------------------------------------------
main :: IO ()
main = do
  b      <- BSL.readFile "collabdata/model.json"
  jqPath <- JQuery.file
  case A.decode b of
    Nothing -> error "Json decode error"
    Just m  -> do
      L.renderToFile "collaborations.html" $
        page jqPath m (svg (bkgnd <> modelSvg m))


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

svg :: LSvg.Svg () -> LSvg.Svg ()
svg content = do
  LSvg.doctype_
  LSvg.with (LSvg.svg11_ (gTranslate 400 400 content))
    [LSvg.version_ "1.1", LSvg.width_ "800", LSvg.height_ "800"]
