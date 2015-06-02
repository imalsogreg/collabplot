{-# LANGUAGE OverloadedStrings #-}

module Shadow where

import Data.Foldable
import qualified Data.Text as T
import Lucid.Svg
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements   as E


shadowDefs :: Int -> Int -> Int -> T.Text -> Svg ()
shadowDefs x y blur filtId = defs_ $ do
  (term "filter") fParams $ do
    feOffset_       [result_ "offOut", in_ "SourceAlpha"
                    , dx_ (T.pack $ show x), dy_ (T.pack $ show y)]
    feColorMatrix_  [result_ "matrixOut", in_ "offOut"
                    , type_ "matrix"
                    , values_ "1 0 0 0 0   0 1 0 0 0  0 0 1 0 0  0 0 0 1 0"]
    --feFlood_        [result_ "floodOut", in_ "offOut"
    --                , flood_color_ "rgba(0,0,0,0.1)"
    --                , flood_opacity_ "1"]
    feGaussianBlur_ [result_ "blurOut", in_ "matrixOut"
                    , stdDeviation_ (T.pack $ show blur)]
    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
                    , mode_ "normal"]
  where
    fParams = [ id_ filtId , x_ "-0.5" , y_ "-0.5"
              , width_ "200%" , height_ "200%"]

--svg :: Svg () -> Svg ()
--svg content = do
--  with (svg11_ (content))
--    [version_ "1.1", width_ "700", height_ "500"]

dropShadow x y blur el = do
  shadowDefs x y blur filtName
  with el [A.filter_ filtUrl]
  where filtName = T.pack $ "shadowFiltX" ++ show x ++ "Y" ++ show y
                            ++ "B" ++ show blur
        filtUrl = T.concat ["url(#", filtName, ")"]
