{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Concurrent
import Control.Monad (liftM)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.Fixed                 (mod')
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Traversable
import           Figure
import           Lucid                      as L
import qualified Lucid.Svg                  as LSvg

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.HTMLDocument
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import Reflex.Dom

import           Model
import           Page
import           Primitives
import           Shadow
import           Utils
import           Menus

------------------------------------------------------------------------------
main :: IO ()
main = do
  runWebGUI $ \webView -> do
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
           webViewGetDomDocument webView
    let btag = "reflex-area" :: String
    root <- waitUntilJust $ liftM (fmap castToHTMLElement) $
            documentGetElementById doc btag
    attachWidget root webView pageWidget

waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
  mx <- a
  case mx of
    Just x -> return x
    Nothing -> do
      threadDelay 10000
      waitUntilJust a
