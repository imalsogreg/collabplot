{-# LANGUAGE OverloadedStrings #-}

module Ingest where

import Control.Monad (mzero)
import Data.Csv
import Data.Aeson
import qualified Data.Text as T
import Model

{- 
instance FromRecord Thrust
instance FromNamedRecord Thrust
instance ToRecord Thrust
instance ToNamedRecord Thrust
instance FromRecord PI
instance ToRecord PI

instance FromRecord Project where
  parseRecord v
    | length v >= 2 = Project
                      <$> v .! 0
                      <*> fmap (map T.strip . T.splitOn ";") (v .! 1)
                      <*> v .! 2
    | otherwise = mzero

instance ToRecord Project where
  toRecord (Project pName mNames gitHub) = record [
      toField pName
    , toField (T.intercalate "; " mNames)
    , toField gitHub
    ]

-}
