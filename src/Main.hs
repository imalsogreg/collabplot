{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Fixed           (mod')
import           Data.Monoid
import           Data.Traversable
import qualified Data.Text            as T
import           Lucid.Svg
import qualified Lucid.Svg            as L
import           Primitives
import           Shadow
import           Utils
import           Ingest
import           Model


------------------------------------------------------------------------------
thrusts :: [T.Text]
thrusts = ["Development","Circuits","Social","Theory","Vision"]


------------------------------------------------------------------------------
main :: IO ()
main = L.renderToFile "test.svg" . svg $ do

  bkgnd

  let n            = fromIntegral $ length thrusts
      tWedge i tn  = textWedge
                     (TextWedge tn (i*(2*pi/n)) (2*pi/n - 0.05)
                      150 200 [] [fill_ "hsl(150,50%,75%)", stroke_ "none"])
  dropShadow 2 2 4 "black" $ g_ $ mconcat $ zipWith tWedge [0..] thrusts

  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Kanwisher"  0      0.25 210 350
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])
  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Tennenbaum" (pi/1.2) 0.25 210 350
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])
  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Saxe" (pi/4.2) 0.25 210 350
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])

  dropShadow 0 0 2 "yellow" $ with (highLine 0 (pi/1.2) 350 400)
    [fill_ "none", stroke_ "yellow", stroke_width_ "2px"]
  dropShadow 0 0 2 "yellow" $ with (highLine 0 (pi/4.2) 350 400)
    [fill_ "none", stroke_ "yellow", stroke_width_ "2px"]
  dropShadow 0 0 2 "yellow" $ with (highLine (pi/4.2) (pi/1.2) 350 400)
    [fill_ "none", stroke_ "yellow", stroke_width_ "1px"]

------------------------------------------------------------------------------
data TextWedge = TextWedge {
    twText       :: T.Text
  , twTheta      :: Double
  , twWidth      :: Double
  , twInner      :: Double
  , twOuter      :: Double
  , twTextAttrs  :: [Attribute]
  , twBkgndAttrs :: [Attribute]
  } deriving (Eq, Show)


------------------------------------------------------------------------------
textWedge :: TextWedge -> Svg ()
textWedge TextWedge{..} = g_ $ do
  (with $ taurusWedge
   (TaurusWedgeSpec 0 0
    twInner twOuter twTheta twWidth) False)
    twBkgndAttrs
  textOnCircle twText twTextAttrs (twInner/2 + twOuter/2) twTheta Nothing

textWedge' :: TextWedge -> Svg ()
textWedge' TextWedge{..} = g_ $ do
  (with $ taurusWedge
   (TaurusWedgeSpec 0 0
    twInner twOuter twTheta twWidth) True)
    twBkgndAttrs
  textOnCircle twText twTextAttrs (twOuter/2 + twInner/2)
    twTheta (Just $ twOuter - twInner)

svgHeight, svgWidth :: Double
svgHeight = 800
svgWidth  = 800

bkgnd :: Svg ()
bkgnd = do
  defs_ $ do
    radialGradient_ [id_ "bkgndGradient"
                    , cx_ "0.6"
                    , cy_ "0.6"
                    , r_  "0.6"] $ do
      stop_ [offset_ "0%", stop_color_ "#1b5354"]
      stop_ [offset_ "100%", stop_color_ "#0f2d2d"]
  rect_ [x_ (f (svgWidth/(-2))), y_ (f (svgHeight/(-2)))
        , width_ (f svgWidth), height_ (f svgHeight)
        , fill_ "url(#bkgndGradient)"]

svg :: L.Svg () -> L.Svg ()
svg content = do
  L.doctype_
  L.with (L.svg11_ (gTranslate 400 400 content))
    [L.version_ "1.1", L.width_ "800", L.height_ "800"]

