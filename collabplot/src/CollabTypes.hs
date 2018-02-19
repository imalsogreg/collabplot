{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CollabTypes where

import Control.Lens
import Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson
import qualified Data.Text as T
import Data.UUID
import qualified Data.UUID as UUID
import GHC.Generics


data Member = Member {
    _memberID :: UUID
  , _memberName :: T.Text
  , _memberPI :: UUID
  , _memberSite :: Maybe T.Text
  }deriving (Eq, Show, Ord, Generic)

instance A.FromJSON Member where
instance A.ToJSON   Member where


data InsMember = InsMember {
    imemberName :: T.Text
  , imemberPI :: UUID.UUID
  , imemberSite :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON InsMember where
instance ToJSON   InsMember where

data Thrust = Thrust {
    _thrustID :: UUID
  , _thrustName :: T.Text
  , _thrustPIs :: [PI]
  } deriving (Eq, Show, Ord, Generic)

instance A.FromJSON Thrust where
instance A.ToJSON Thrust where


data InsThrust = InsThrust {
  ithrustName :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON InsThrust where
instance ToJSON   InsThrust where


data PI = PI {
    _piID :: UUID
  , _piName :: T.Text
  , _piThrust :: UUID
  , _piSite :: Maybe T.Text
  , _piMembers :: [Member]
  }deriving (Eq, Show, Ord, Generic)

instance A.FromJSON PI where
instance A.ToJSON   PI where

makeLenses ''PI

data InsPI = InsPI {
    ipiName :: T.Text
  , ipiThrust :: UUID.UUID
  , ipiSite :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON InsPI where
instance A.FromJSON InsPI where




data Project = Project {
    _projectID :: UUID
  , _projectName :: T.Text
  , _projectSite :: Maybe T.Text
  , _projectMembers :: [Member]
  }deriving (Eq, Show, Ord, Generic)

instance A.FromJSON Project where
instance A.ToJSON   Project where


data InsProject = InsProject {
    iprojectName :: T.Text
  , iprojectSite :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON InsProject where
instance ToJSON   InsProject where





data Model = Model {
    _modelThrusts    :: [Thrust]
  , _modelProjects   :: [Project]

  } deriving (Eq, Show, Generic)

instance A.FromJSON Model where
instance A.ToJSON   Model where


instance A.FromJSON UUID where
  parseJSON (A.String t) = case UUID.fromString (T.unpack t) of
    Just u -> return u
    Nothing -> mzero
  parseJSON _ = mzero

instance A.ToJSON UUID where
  toJSON = A.String . T.pack . UUID.toString


makeLenses ''Model
makeLenses ''Project
makeLenses ''Member
makeLenses ''Thrust
