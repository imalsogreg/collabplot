{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Page where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JMacro
import qualified Language.Javascript.JQuery as JQuery
import Lucid
import qualified Lucid as L
import Lucid.Svg (Svg(..))
import Model
import Utils
import Network.HTTP.Base (urlEncode, urlDecode)


------------------------------------------------------------------------------
page :: FilePath -> Model -> Svg () -> Html ()
page jsPath Model{..} figSvg = html_ $ do  -- Todo drop filepath thing
  doctype_
  head_ $ do
    link_ [rel_ "stylesheet", type_ "text/css", href_ "default.css"]
    --script_ [src_ (T.pack jsPath)]
    (termWith "script" [src_ "http://code.jquery.com/jquery-2.1.4.min.js"] (return ()))
  body_ $ do
    div_ [class_ "main-figure"] $ figSvg
    forM_ projects $ \p -> do
      div_ [class_ "synopsis"
           ,id_    (textEncode $ projectName p)] $ do
        table_ $ do
          tr_ $ td_ [class_ "field"] "Project name:" <> td_ [class_ "val"] (toHtml $ projectName p)
          tr_ $ td_ [class_ "field"] "Members:"      <> td_ [class_ "val"] (foldMap toHtml $ projectMembers p)
    (termWith "script" [src_ (T.pack "collab.js")] (return ()))

--collabClick = renderJs [$jmacro| $(".collabLine").on("click", function(){
--    console.log( $(this).text() );
--  });
-- |]
