{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CollabTypes where

import Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson
import qualified Data.Text as T
import Data.UUID
import GHC.Generics
import Snap.Snaplet.PostgresqlSimple

data Model = Model {
    modelThrusts :: [Thrust]
  , modelProjects :: [Project]
  }

data Thrust = Thrust {
    thrustID :: UUID
  , thrustName :: T.Text
  , thrustPIs :: [PI]
  } deriving (Eq, Show, Generic)

instance ToRow Thrust where
  toRow (Thrust tID tName _) = toRow (tID, tName)

instance FromRow Thrust where
  fromRow = Thrust <$> field <*> field <*> pure []

instance A.FromJSON Thrust where
instance A.ToJSON Thrust where

data PI = PI {
    piID :: UUID
  , piName :: T.Text
  , piThrust :: UUID
  , piSite :: T.Text
  , piMembers :: [Member]
  }deriving (Eq, Show, Generic)

instance ToRow PI where
  toRow PI{..} = toRow (piID, piName, piThrust, piSite)

instance FromRow PI where
  fromRow = PI <$> field <*> field <*> field <*> field <*> pure []

instance A.FromJSON PI where
instance A.ToJSON   PI where

data Member = Member {
    memberID :: UUID
  , memberName :: T.Text
  , memberPI :: UUID
  }deriving (Eq, Show, Generic)

instance FromRow Member where
  fromRow = Member <$> field <*> field <*> field

instance ToRow Member where
  toRow Member{..} = toRow (memberID, memberName, memberPI)

instance A.FromJSON Member where
instance A.ToJSON   Member where


data Project = Project {
    projectID :: UUID
  , projectName :: T.Text
  , projectMembers :: [Member]
  }deriving (Eq, Show, Generic)

instance ToRow Project where
  toRow Project{..} = toRow (projectID, projectName)

instance FromRow Project where
  fromRow = Project <$> field <*> field <*> pure []

instance A.FromJSON Project where
instance A.ToJSON   Project where

instance A.FromJSON UUID where
  parseJSON (A.Object v) = do
    [a,b,c,d] <- v .: "uuidWords"
    return $ fromWords a b c d
  parseJSON _ = mzero

instance A.ToJSON UUID where
  toJSON u = let (a,b,c,d) = toWords u
             in A.object ["uuidWords" .= [a,b,c,d]]
