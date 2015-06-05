{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Control.Monad (mzero)
import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.JSON.Schema as JS
import Data.Maybe
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
               deriving (Eq, Ord, Show, Generic,
                         ToJSON, FromJSON, JS.JSONSchema)

data PI = PI { piName   :: T.Text
             , piThrust :: T.Text
             }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON PI where
  toJSON (PI n t) = object ["name" .= n, "thrust" .= t]
instance FromJSON PI where
  parseJSON (Object v) = PI <$> v .: "name" <*> v.: "thrust"
  parseJSON _          = mzero

-- instance JS.JSONSchema PI where schema = JS.gSchema

data MemberGroup = MemberPI     T.Text
                 | MemberThrust T.Text
                   deriving (Eq, Ord, Show)

--instance ToJSON MemberGroup where
--  toJSON (MemberPI mPi)   = "pi"     .= mPi
--  toJSON (MemberThrust t) = "thrust" .= t

--instance FromJSON MemberGroup where
--  parseJSON (Object v) = case HM.keys v of
--    ["pi"]     -> MemberPI     <$> v .: "pi"
--    ["thrust"] -> MemberThrust <$> v .: "thrust"
--    _          -> mzero
--  parseJSON _          = mzero

data Member = Member {
    memberName     :: !T.Text
  , memberGroup    :: MemberGroup
  , memberGithubId :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Member where
  toJSON (Member n grp g) =
    object . catMaybes $ [Just ("name" .= n)
                         , Just grpFld
                         , ("githubId" .=) <$> g
                         ]
    where grpFld = case grp of
            MemberPI p     -> "pi"     .= p
            MemberThrust t -> "thrust" .= t

instance FromJSON Member where
  parseJSON (Object v) = do
    n   <- v .: "name"
    mPi <- v .:? "pi"
    mTh <- v .:? "thrust"
    mGH <- v .:? "githubId"
    case (mPi,mTh) of
     (Nothing, Nothing) -> mzero
     (Just _,  Just _ ) -> mzero
     (Just p,  Nothing) -> return $ Member n (MemberPI p)     mGH
     (Nothing, Just t ) -> return $ Member n (MemberThrust t) mGH
  parseJSON _ = mzero

data Project = Project {
    projectName :: !T.Text
  , projectMembers :: [T.Text]
  , projectGithubUrl :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Project where
  toJSON (Project n ms g) = object . catMaybes $
                            [Just ("name" .= n)
                            ,Just ("memberNames" .= ms)
                            ,("githubUrl" .=) <$> g
                            ]

instance FromJSON Project where
  parseJSON (Object v) =
    Project
    <$> v .: "name"
    <*> v .: "memberNames"
    <*> v .:? "githubUrl"
  parseJSON _ = mzero
