{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import GHC.Generics
import qualified Data.Text as T
import Data.Csv

newtype Thrust = Thrust { unThrust :: T.Text }
               deriving (Eq, Ord, Show, Generic, ToField, FromField)

newtype PI = PI { unPI :: T.Text }
           deriving (Eq, Ord, Show, Generic, ToField, FromField)

data Member = Member {
    memberName :: !T.Text
  , memberPI :: !PI
  , memberGithubId :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

data Project = Project {
    projectName :: !T.Text
  , projectMembers :: [T.Text]
  , projectGithubUrl :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)
