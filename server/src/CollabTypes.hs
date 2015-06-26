{-# LANGUAGE DeriveGeneric #-}

module CollabTypes where

import qualified Data.Text as T
import Data.UUID
import GHC.Generics

data Model = Model {
    modelThrusts :: [Thrust]
  , modelProjects :: [Project]
  }

data Thrust = Thrust {
    thrustID :: UUID
  , thrustName :: T.Text
  , thrustPIs :: [PI]
  }

data PI = PI {
    piID :: UUID
  , piName :: T.Text
  , piSite :: T.Text
  , piMembers :: [Member]
  }

data Member = Member {
    memberID :: UUID
  , memberName :: T.Text
  , memberPI :: UUID
  , memberProjs :: [Project]
  }

data Project = Project {
    projectID :: UUID
  , projectName :: T.Text
  , projectMembers :: [Member]
  }
