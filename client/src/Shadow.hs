{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Shadow where

import Data.Foldable
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Text as T
import Lucid.Svg
import qualified Lucid.Svg.Attributes as A
import qualified Lucid.Svg.Elements   as E
import Reflex
import Reflex.Dom
import Reflex.Dynamic.TH
import Utils


shadowDefs :: Double -> Double -> Double -> T.Text -> T.Text -> Svg ()
shadowDefs x y blur color filtId = defs_ $ do
  (term "filter") fParams $ do
    feOffset_       [result_ "offOut", in_ "SourceAlpha"
                    , dx_ (f x), dy_ (f y)]
    --feColorMatrix_  [result_ "matrixOut", in_ "offOut"
    --                , type_ "matrix"
    --                , values_ "1 0 0 0 0   0 1 0 0 0  0 0 1 0 0  0 0 0 1 0"]
    feFlood_        [result_ "floodOut"
                    , flood_color_ color
                    , flood_opacity_ "1"]
    feGaussianBlur_ [result_ "blurOut", in_ "offOut"
                    , stdDeviation_ (f blur)]
    feComposite_    [result_ "shadowOut"
                    ,in_ "floodOut", in2_ "blurOut", operator_ "in"]
    feBlend_        [in_ "SourceGraphic", in2_ "shadowOut", mode_ "normal"]
--    feGaussianBlur_ [result_ "blurOut", in_ "floodOut"
--                    , stdDeviation_ (f blur)]
--    feBlend_        [in_ "SourceGraphic", in2_ "blurOut"
--                    , mode_ "normal"]
  where
    fParams = [ id_ filtId , x_ "-0.5" , y_ "-0.5"
              , width_ "200%" , height_ "200%"]

r = f 5

dropShadow x y blur color el = do
  shadowDefs x y blur color filtName
  with el [A.filter_ filtUrl]
  where filtName = mconcat [ "shadowFiltX", f x, "Y", f y
                           , "B", f blur, "C", color]
        filtUrl = T.concat ["url(#", filtName, ")"]

filtID :: MonadWidget t m => ShadowParams t -> m (Dynamic t String)
filtID sp =
  $(qDyn [| mconcat ["shadowFiltX", fs      $(unqDyn[| _spX sp |])
                    ,"Y",           fs      $(unqDyn[| _spY sp |])
                    ,"B",           fs      $(unqDyn[| _spBlur sp |])
                    ,"C",           doColor $(unqDyn[| _spColor sp |])
                    ]
         |])
  where doColor = filter (`notElem` ['(',')',','])

data ShadowParams t = ShadowParams
  { _spX     :: Dynamic t Int
  , _spY     :: Dynamic t Int
  , _spBlur  :: Dynamic t Int
  , _spColor :: Dynamic t String
  }

--defShadowParams :: MonadWidget t m => m (ShadowParams t)
defShadowParams :: Reflex t => ShadowParams t
defShadowParams = ShadowParams
  (constDyn 4) (constDyn 4) (constDyn 1) (constDyn "rgba(0,0,0,1)")

elShadow' :: MonadWidget t m
          => ShadowParams t
          -> m a
          -> m a
          -- -> String
          -- -> Dynamic t (Map String String)
          -- -> m (El t)
elShadow' sp@ShadowParams{..} child = do
-- elShadow' sp@ShadowParams{..} nm attrs = do

  fID  <- filtID sp
  fURL <- forDyn fID $ \n -> "url(#" ++ n ++ ")"

  filtElemOffsetAttrs <- combineDyn (\x y -> "result" =: "offOut"
                                          <> "in"     =: "SourceAlpha"
                                          <> "dx"     =: fs x
                                          <> "dy"     =: fs y
                                    ) _spX _spX

  filtElemFloodAttrs <- forDyn _spColor $ \c ->
    (  "result" =: "floodOut"
    <> "flood-color" =: c
    <> "flood-opacity" =: "1" )

  filtElemBlurAttrs <- forDyn _spBlur $ \r ->
       "result" =: "blurOut"
    <> "in" =: "offOut"
    <> "stdDeviation" =: show r

  let filtElemCompositeAttrs = ("result" =: st "shadowOut")
                               <> ("in" =: "floodOut") <> ("in2" =: "blurOut")
                               <> "operator" =: "in"

      filtElemBlendAttrs = ("in" =: "SourceGraphic") <> ("in2" =: "shadowOut")
                           <> ("mode" =: st "normal")

  filtElemAttrs <- forDyn fID $ \n ->
       "id"    =: n
    <> "x"     =: "-0.5" <> "y"      =: "-0.5"
    <> "width" =: "200%" <> "height" =: "200%"

  gAttrs <- mapDyn ("filter" =: ) fURL

  filtElem <- svgElDynAttr "filter" filtElemAttrs $ do
    svgElDynAttr "feOffset"       filtElemOffsetAttrs    $ return ()
    svgElDynAttr "feFlood"        filtElemFloodAttrs     $ return ()
    svgElDynAttr "feGaussianBlur" filtElemBlurAttrs      $ return ()
    svgElAttr    "feComposite"    filtElemCompositeAttrs $ return ()
    svgElAttr    "feBlend"        filtElemBlendAttrs     $ return ()
  e <- svgElDynAttr "g" gAttrs $ child
  --(e, ()) <- svgElDynAttr' nm shadowedElAttrs $ return ()
  return e
