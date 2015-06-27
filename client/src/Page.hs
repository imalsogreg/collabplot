{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Lucid.Svg (Svg(..), renderText)
import CollabTypes
import Utils
import Network.HTTP.Base (urlEncode, urlDecode)
import Figure
import Reflex
import Reflex.Dom

pageWidget :: (MonadWidget t m) => m ()
pageWidget = do

  pb <- getPostBuild
  let  modelUrls = "localhost:8000/thrusts" <$ pb

  --let getDataReq = xhrRequest "GET" "localhost:8000/thrusts" def -- TODO, also get projects
  --dataEvents  <- performEventAsync getDataReq (leftmost [pb])
  --modelEvents <- fmapMaybe (A.decode) dataEvents
  modelEvents <- fmapMaybe id <$> getAndDecode modelUrls
  model       <- holdDyn (Model [] []) modelEvents

  -- menuEvents <- menusWidget pictureEvents
  --infoWidget menuEvents

  elClass "div" "main-figure" $ do
    svgDyn =<< (mapDyn modelSvg model)
    --pictureEvents <- pictureWidget menuEvents
  return ()

svgDyn :: (MonadWidget t m) => Dynamic t (Svg ()) -> m ()
svgDyn s = do
  elDynHtml' "div" =<< mapDyn (TL.unpack . renderText) s
  return ()


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

