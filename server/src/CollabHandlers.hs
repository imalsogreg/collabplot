{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric    #-}

module CollabHandlers where

import Control.Applicative
import Snap.Core
import Snap.Snaplet
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Snap.Snaplet.PostgresqlSimple

import Control.Monad (when)
import qualified Data.Aeson as A
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as UUID
import           GHC.Generics

import Snap.Snaplet.Auth
import Application
import CollabTypes

requireMod :: Handler App (AuthManager App) ()
requireMod = do
  u <- currentUser
  mods <- withTop db $ query_ "SELECT * FROM mods;"
  when (Only (maybe "" userLogin u) `notElem` mods) (badReq "Access denied")

getModel :: Handler App (AuthManager App) Model
getModel = do
  thrusts  <- getThrusts
  projects <- pure []
  return $ Model thrusts projects

getThrusts :: Handler App (AuthManager App) [Thrust]
getThrusts = do
  thrusts <- withTop db $ query_ "SELECT * FROM thrust;"
  pis     <- getPIs
  return $ flip map thrusts $ \t@Thrust{..} ->
    t { _thrustPIs = filter (\PI{..} -> _piThrust == _thrustID) pis}

getPIs :: Handler App (AuthManager App) [PI]
getPIs = do
  pis     <- withTop db $ query_ "SELECT * FROM pi;"
  members <- getMembers
  return $ flip map pis $ \p@PI{..} ->
    p { _piMembers = filter (\Member{..} -> _memberPI == _piID) members}

getMembers :: Handler App (AuthManager App) [Member]
getMembers = do
  withTop db $ query_ "SELECT * FROM member;"

getProjects :: Handler App (AuthManager App) [Project]
getProjects = withTop db $ query_ "SELECT * FROM project;"

handleThrusts :: Handler App (AuthManager App) ()
handleThrusts = method GET (getThrusts >>= writeJSON)
                <|> method POST (requireMod >> postThrust)
  where postThrust = do
          InsThrust{..} <- reqJSON
          [Only i] <- withTop db $ query "INSERT INTO thrust(name) VALUES (?)" (Only thrustName)
          writeJSON (Thrust i thrustName [])

handleModel :: Handler App (AuthManager App) ()
handleModel = do
  getModel >>= writeJSON

handleThrust :: Handler App (AuthManager App) ()
handleThrust = do
  idBytes <- reqParam "id"
  tID <- maybe (badReq "No id parse") return (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deleteThrust tID) <|> method PUT (requireMod >> putThrust tID)
  where deleteThrust i = do
          withTop db $ execute "DELETE FROM thrust WHERE id=(?)" (Only i)
          writeBS "Ok"
        putThrust i = do
          (Thrust i nm pis) <- reqJSON
          [Only r] <- withTop db $ query "UPDATE thrust WITH name=(?) WHERE id=(?);"
                      (nm, i)
          writeJSON (Thrust r nm pis)

handleProjects :: Handler App (AuthManager App) ()
handleProjects = method GET (getProjects >>= writeJSON)
                 <|> method POST (requireMod >> postProject)
  where postProject = do
          (InsProject nm ws) <- reqJSON
          [Only r] <- withTop db $
            query "INSERT INTO project(name,website) VALUES (?,?) returning id;"
            (nm, ws)
          writeJSON (Project r nm ws [])

handleProject :: Handler App (AuthManager App) ()
handleProject = do
  idBytes <- reqParam "id"
  pID <- maybe (badReq "No id parse") return (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deleteProject pID) <|> method PUT (requireMod >> putProject pID)
  where deleteProject i = do
          withTop db $ execute "DELETE FROM project WHERE id=(?);" (Only i)
          writeBS "Ok."
        putProject i = do
          Project{..} <- reqJSON
          [Only r] <- withTop db $ query "UPDATE project WITH name=(?), site=(?) WHERE id=(?);"
                      (_projectName, _projectSite, i)
          writeJSON (Project r _projectName _projectSite _projectMembers)

handleMembers :: Handler App (AuthManager App) ()
handleMembers = method GET (getMembers >>= writeJSON)
                <|> method POST (requireMod >> insertMember)
  where insertMember = do
          (InsMember mn mPI ws) <- reqJSON
          [Only i] <- withTop db $ query
                      "INSERT INTO member(name,pi,website) VALUES (?,?,?) returning id;"
                      (mn, mPI, ws)
          writeJSON (Member i mn mPI ws)

handleMember :: Handler App (AuthManager App) ()
handleMember = do
  idBytes <- reqParam "id"
  mID <- maybe (badReq "No id parse") return (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deleteMem mID) <|> method PUT (requireMod >> putMem mID)
  where deleteMem i = do
          withTop db $ execute "DELETE FROM member WHERE id=(?)"
            (Only i)
          writeBS "Ok."
        putMem i = do
          (InsMember nm mPi ws) <- reqJSON
          [Only r] <- withTop db $ query
                      "UPDATE member SET name=(?),pi=(?),website=(?) WHERE id=(?)"
                      (nm, mPi, ws, i)
          writeJSON (Member r nm mPi ws)


handlePIs :: Handler App (AuthManager App) ()
handlePIs = method GET (getPIs >>= writeJSON)
            <|> method POST (requireMod >> insertPI)
  where insertPI = do
          (InsPI nm th ws) <- reqJSON
          [Only i] <- withTop db $ query
                      "INSERT INTO pi(name,thrust,website) VALUES (?,?,?) returning id;"
                      (nm,(th :: UUID.UUID),ws)
          writeJSON (PI i nm th ws [])


handlePI :: Handler App (AuthManager App) ()
handlePI = do
  idBytes <- reqParam "id"
  piID <- maybe (badReq "No parse for id") return
          (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deletePI piID) <|> method PUT (requireMod >> putPI piID)
    where
      deletePI i = do
        withTop db $ execute "DELETE FROM pi WHERE id=(?)" (Only i)
        writeBS "Ok."
      putPI    i = do
        PI{..} <- reqJSON
        [Only r] <- withTop db $ query
          "UPDATE pi SET name=(?), thrust=(?), site=(?) WHERE id=(?);"
          (_piName, _piThrust, _piSite, (i :: UUID.UUID))
        writeJSON (PI r _piName _piThrust _piSite _piMembers)

instance ToRow PI where
  toRow PI{..} = toRow (_piID, _piName, _piThrust, _piSite)

instance FromRow PI where
  fromRow = PI <$> field <*> field <*> field <*> field <*> pure []



instance FromRow Member where
  fromRow = Member <$> field <*> field <*> field <*> field

instance ToRow Member where
  toRow Member{..} = toRow (_memberID, _memberName, _memberPI, _memberSite)


instance ToRow Project where
  toRow Project{..} = toRow (_projectID, _projectName, _projectSite)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field <*> pure []

instance ToRow Thrust where
  toRow (Thrust tID tName _) = toRow (tID, tName)

instance FromRow Thrust where
  fromRow = Thrust <$> field <*> field <*> pure []

