{-# LANGUAGE OverloadedStrings #-}

module Shadow where

import Data.Foldable
import qualified Data.Text as T
import Lucid.Svg
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements   as E
import Utils


shadowDefs :: Double -> Double -> Double -> T.Text -> T.Text -> Svg ()
shadowDefs x y blur color filtId = defs_ $ do
  (term "filter") fParams $ do
    feOffset_       [result_ "offOut", in_ "SourceAlpha"
                    , dx_ (f x), dy_ (f y)]
    --feColorMatrix_  [result_ "matrixOut", in_ "offOut"
    --                , type_ "matrix"
    --                , values_ "1 0 0 0 0   0 1 0 0 0  0 0 1 0 0  0 0 0 1 0"]
    feFlood_        [result_ "floodOut", in_ "offOut"
                    , flood_color_ color
                    , flood_opacity_ "1"]
    feGaussianBlur_ [result_ "blurOut", in_ "floodOut"
                    , stdDeviation_ (f blur)]
    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
                    , mode_ "normal"]
  where
    fParams = [ id_ filtId , x_ "-0.5" , y_ "-0.5"
              , width_ "200%" , height_ "200%"]


dropShadow x y blur color el = do
  shadowDefs x y blur color filtName
  with el [A.filter_ filtUrl]
  where filtName = mconcat [ "shadowFiltX", f x, "Y", f y
                           , "B", f blur, "C", color]
        filtUrl = T.concat ["url(#", filtName, ")"]
