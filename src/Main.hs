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
defThrusts :: [T.Text]
defThrusts = ["Development","Circuits","Social","Theory","Vision"]


------------------------------------------------------------------------------
main :: IO ()
main = L.renderToFile "test.svg" . svg $ do

  bkgnd

  let n            = fromIntegral $ length defThrusts
      tWedge i tn  = textWedge
                     (TextWedge tn (i*(2*pi/n)) (2*pi/n - 0.05)
                      (piBig + 5) (piBig + 70) [] [fill_ "hsl(150,50%,75%)", stroke_ "none"])
      piBig        = 250
      piSmall      = 100
      thetaTenn    = 3*pi/2
      thetaKanw    = 0
      thetaSaxe    = pi/1.2
  dropShadow 2 2 4 "black" $ g_ $ mconcat $ zipWith tWedge [0..] defThrusts

  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Kanwisher"  thetaKanw  0.25 piSmall piBig
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])
  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Tennenbaum" thetaTenn 0.25 piSmall piBig
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])
  dropShadow 2 2 4 "black" $
    textWedge' (TextWedge "Saxe" thetaSaxe 0.25 piSmall piBig
                [] [fill_ "hsl(100,50%,50%)", stroke_ "none"])

  dropShadow 0 0 2 "yellow" $ with (highLine thetaKanw thetaTenn piBig 100)
    [fill_ "none", stroke_ "yellow", stroke_width_ "2px"]
  dropShadow 0 0 2 "yellow" $ with (highLine thetaKanw thetaSaxe piBig 100)
    [fill_ "none", stroke_ "yellow", stroke_width_ "2px"]
  dropShadow 0 0 2 "yellow" $ with (highLine thetaTenn thetaSaxe piBig 100)
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

