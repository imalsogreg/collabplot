{-# LANGUAGE OverloadedStrings #-}
module CollabHandlers where

import Snap.Core
import Snap.Snaplet
import Snap.Extras.JSON

import qualified Data.Aeson as A
import           Data.Aeson

import Application

handleThrusts :: Handler App App ()
handleThrusts = do
  undefined

handleThrust :: Handler App App ()
handleThrust = undefined
