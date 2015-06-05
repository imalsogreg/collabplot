{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Figure where

import qualified Data.Monoid
import qualified Data.Text as T
import Lucid.Svg
import Model

data FigOpts = FigOpts {
    piMinWedgeWidth :: Double
  , piWedgeSpacing :: Double
  , thrustWedgeSpacing :: Double
  , piRadiusMin :: Double
  , piRadiusMax :: Double
  , thrustRadiusMin :: Double
  , thrustRadiusMax :: Double
  , collabRadius :: Double
  }

modelSvg :: Model -> Svg ()
modelSvg Model{..} = 
