{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Control.Monad (mzero)
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

------------------------------------------------------------------------------
data Model = Model {
  thrusts    :: [Thrust]
  , pis      :: [PI]
  , members  :: [Member]
  , projects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON   Model where
instance FromJSON Model where

newtype Thrust = Thrust { unThrust :: T.Text }
               deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype PI = PI { unPI :: T.Text }
           deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Member = Member {
    memberName     :: !T.Text
  , memberPI       :: Maybe PI
  , memberThrust   :: Maybe Thrust
  , memberGithubId :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Member where
  toJSON (Member n p t g) = object ["name" .= n
                                   , "pi" .= p
                                   , "thrust" .= t
                                   , "githubId" .= g
                                   ]

instance FromJSON Member where
  parseJSON (Object v) =
    Member
    <$> v .: "name"
    <*> v .: "pi"
    <*> v .: "thrust"
    <*> v .: "githubId"
  parseJSON _ = mzero

data Project = Project {
    projectName :: !T.Text
  , projectMembers :: [T.Text]
  , projectGithubUrl :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Project where
  toJSON (Project n ms g) = object ["name" .= n
                                   ,"members" .= ms
                                   ,"githubUrl" .= g
                                   ]

instance FromJSON Project where
  parseJSON (Object v) =
    Project
    <$> v .: "name"
    <*> v .: "members"
    <*> v .: "githubUrl"
  parseJSON _ = mzero
