{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric    #-}

module CollabHandlers where

import Control.Applicative
import Control.Error
import Snap.Core
import Snap.Snaplet
import Snap.Extras.CoreUtils
import Snap.Extras.JSON
import Snap.Snaplet.PostgresqlSimple

import Control.Monad (when)
import qualified Data.Aeson as A
import           Data.Aeson
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
  mods <- withTop db $ query_ "SELECT * FROM mod;"
  when (Only (maybe "" userLogin u) `notElem` mods) (badReq "Access denied")

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
          writeBS (UUID.toASCIIBytes i)

handleThrust :: Handler App (AuthManager App) ()
handleThrust = do
  idBytes <- reqParam "id"
  tID <- maybe (badReq "No id parse") return (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deleteThrust tID) <|> method PUT (requireMod >> putThrust tID)
  where deleteThrust i = do
          withTop db $ execute "DELETE FROM thrust WHERE id=(?)" (Only i)
          writeBS "Ok"
        putThrust i = do
          (InsThrust nm) <- reqJSON
          [Only r] <- withTop db $ query "UPDATE thrust WITH name=(?) WHERE id=(?);"
                      (nm, i)
          writeBS (UUID.toASCIIBytes r)

data InsThrust = InsThrust {
  thrustName :: T.Text
  } deriving (Generic)

instance FromJSON InsThrust where
instance ToJSON   InsThrust where

handleProjects :: Handler App (AuthManager App) ()
handleProjects = method GET (getProjects >>= writeJSON)
                 <|> method POST (requireMod >> postProject)
  where postProject = do
          (InsProject nm ws) <- reqJSON
          [Only i] <- withTop db $ query "INSERT INTO project(name,website);"
                      (nm, ws)
          writeBS (UUID.toASCIIBytes i)

handleProject :: Handler App (AuthManager App) ()
handleProject = do
  idBytes <- reqParam "id"
  pID <- maybe (badReq "No id parse") return (UUID.fromASCIIBytes idBytes)
  method DELETE (requireMod >> deleteProject pID) <|> method PUT (requireMod >> putProject pID)
  where deleteProject i = do
          withTop db $ execute "DELETE FROM project WHERE id=(?);" (Only i)
          writeBS "Ok."
        putProject i = do
          InsProject{..} <- reqJSON
          [Only r] <- withTop db $ query "UPDATE project WITH name=(?), site=(?) WHERE id=(?);"
                      (projectName, projectSite, i)
          writeBS (UUID.toASCIIBytes r)

data InsProject = InsProject {
  projectName :: T.Text
  , projectSite :: Maybe T.Text
  } deriving (Generic)

instance FromJSON InsProject where
instance ToJSON   InsProject where

handleMembers :: Handler App (AuthManager App) ()
handleMembers = method GET (getMembers >>= writeJSON)
                <|> method POST (requireMod >> insertMember)
  where insertMember = do
          (InsMember mn mPI ws) <- reqJSON
          [Only i] <- withTop db $ query "INSERT INTO member(name,pi,website) returning id;"
                      (mn, mPI, ws)
          writeBS (UUID.toASCIIBytes i)

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
          writeBS (UUID.toASCIIBytes r)

data InsMember = InsMember {
  memberName :: T.Text
  , memberPI :: UUID.UUID
  , memberSite :: Maybe T.Text
  } deriving (Generic)

instance FromJSON InsMember where
instance ToJSON   InsMember where


handlePIs :: Handler App (AuthManager App) ()
handlePIs = method GET (getPIs >>= writeJSON)
            <|> method POST (requireMod >> insertPI)
  where insertPI = do
          (InsPI nm th ws) <- reqJSON
          [Only i] <- withTop db $ query
                      "INSERT INTO pi(name,thrust,website) VALUES (?,?,?) returning id;"
                      (nm,(th :: UUID.UUID),ws)
          writeBS (UUID.toASCIIBytes i)


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
        writeBS (UUID.toASCIIBytes r)

data InsPI = InsPI {
    piName :: T.Text
  , piThrust :: UUID.UUID
  , piSite :: Maybe T.Text
  } deriving (Generic)

instance A.ToJSON InsPI where
instance A.FromJSON InsPI where

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

