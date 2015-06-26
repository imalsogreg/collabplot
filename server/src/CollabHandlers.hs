{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CollabHandlers where

import Snap.Core
import Snap.Snaplet
import Snap.Extras.JSON
import Snap.Snaplet.PostgresqlSimple

import qualified Data.Aeson as A
import           Data.Aeson

import Application
import CollabTypes

getThrusts :: Handler App App [Thrust]
getThrusts = do
  thrusts <- with db $ query_ "SELECT * FROM thrust;"
  pis     <- getPIs
  return $ flip map thrusts $ \t@Thrust{..} ->
    t { thrustPIs = filter (\PI{..} -> piThrust == thrustID) pis}

getPIs :: Handler App App [PI]
getPIs = do
  pis     <- with db $ query_ "SELECT * FROM pi;"
  members <- getMembers
  return $ flip map pis $ \p@PI{..} ->
    p { piMembers = filter (\Member{..} -> memberPI == piID) members}

getMembers :: Handler App App [Member]
getMembers = do
  with db $ query_ "SELECT * FROM member;"

handleThrusts :: Handler App App ()
handleThrusts = do
  getThrusts >>= writeJSON

handleThrust :: Handler App App ()
handleThrust = undefined
