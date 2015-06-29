{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Menus where

import Control.Monad
import qualified Data.Aeson as A
import Data.Bool
import Data.Char (toLower)
import qualified Data.UUID as UUID
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid
import CollabTypes
import Reflex.Dom
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype MField = MField String

instance Monoid MField where
  mempty = MField ""
  MField a `mappend` MField b = MField (a <> ('\n' : b))

------------------------------------------------------------------------------
newPIBox :: (MonadWidget t m) => Dynamic t Model -> m (Event t PI)
newPIBox dModel =
  newEntityBox dModel ["Name:.","Thrust:."] piValidate "/pis"
  where
    piValidate :: Model -> MField -> Maybe InsPI
    piValidate m (MField str) = case lines str of
          [piNm, thNm] ->
            let l = map toLower
                pred = (== l thNm) . l . T.unpack . _thrustName
                mID = _thrustID <$>
                      listToMaybe (filter pred (_modelThrusts m))
            in (\i -> InsPI (T.pack piNm) i Nothing) <$> mID

newMemberBox :: (MonadWidget t m) => Dynamic t Model -> m (Event t Member)
newMemberBox dModel =
  newEntityBox dModel ["Name","PI"] memberValidate "/members"
  where
    memberValidate :: Model -> MField -> Maybe InsMember
    memberValidate m (MField str) = case lines str of
          [memNm, piNm] ->
            let l = map toLower
                allPIs = concatMap _thrustPIs (_modelThrusts m)
                pred = (== l piNm) . l . T.unpack . _piName
                mID = _piID <$>
                      listToMaybe (filter pred allPIs)
            in (\i -> InsMember (T.pack memNm) i Nothing) <$> mID

newProjectBox :: (MonadWidget t m) => Dynamic t Model -> m (Event t Project)
newProjectBox dModel =
  newEntityBox dModel ["Name"] projValidate "/projects"
  where
    projValidate :: Model -> MField -> Maybe InsProject
    projValidate m (MField str) = case lines str of
      [nm] -> case null nm of
        True  -> Nothing
        False -> Just $ InsProject (T.pack nm) Nothing

newEntityBox
  :: forall m t a b.(MonadWidget t m, A.ToJSON a, A.FromJSON b, Show b)
     => Dynamic t Model
     -> [String]
     -> (Model -> MField -> Maybe a)
     -> String
     -> m (Event t b)
newEntityBox dynModel fieldLabels validation route = mdo

  textFields <- forM fieldLabels $ \l ->
    text l *> textInput (def {_textInputConfig_attributes = dynAttrs})

  let enterAttempts = leftmost (map textInputGetEnter textFields)
  -- [TextInput] -> MField
  -- mapM MField . map _textInput_value
  dynEntity <- combineDyn validation dynModel =<<
               mconcatDyn =<<
               (mapM (mapDyn MField) $ map _textInput_value textFields)

  dynAttrs  <- forDyn dynEntity $ \(mayE :: Maybe a) ->
    if not $ isJust mayE
    then "style" =: "background-color:hsl(1,50%,85%);"
    else "style" =: "background-color:hsl(100,50%,85%);"

  dynPost   <- forDyn dynEntity $ \(e :: Maybe a) -> case e of
    Nothing -> Nothing
    Just e' -> Just $
               xhrRequest "POST" route
               (def { _xhrRequestConfig_sendData =
                      (Just . BL.unpack $ A.encode e')
                    , _xhrRequestConfig_headers =
                      ("Content-Type" =: "application/json")
                    })

  let postEvents = fmapMaybe id $ tagDyn dynPost enterAttempts

  r <- performRequestAsync postEvents
  display =<< holdDyn (Just "unused") (fmap (_xhrResponse_body) r)
  return $ fmapMaybe decodeXhrResponse r
