{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Menus where

import Control.Monad
import qualified Data.Aeson as A
import Data.Bool
import Data.Char (toLower)
import qualified Data.UUID as UUID
import Data.Maybe (listToMaybe)
import CollabTypes
import Reflex.Dom
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T


newPIBox :: (MonadWidget t m) => Dynamic t [Thrust] -> m ()
newPIBox dThrusts = mdo

  piInput <- text "Name:"   *>
             textInput (def { _textInputConfig_attributes = dynAttrs})
  thInput <- text "Thrust:" *>
             textInput (def { _textInputConfig_attributes = dynAttrs})
  thrustID <- combineDyn lookupID (_textInput_value thInput) dThrusts

  okToPost <- combineDyn bothOk (_textInput_value piInput) thrustID
  lastCode <- holdDyn 0 (_textInput_keydown thInput)
  dynAttrs <- forDyn okToPost $ bool
              ("style" =: "background-color:hsl(1,50%,85%);")
              ("style" =: "background-color:hsl(100,50%,85%);")


  dynPost <- combineDyn toPost (_textInput_value piInput) =<<
             combineDyn (,) (_textInput_value thInput) dThrusts

  postEvents <- return $ tagDyn dynPost . ffilter id . tagDyn okToPost $
                leftmost [textInputGetEnter piInput ,textInputGetEnter thInput]

  r <- performRequestAsync (fforMaybe postEvents id) --postEvents

  display thrustID
  text " :: "
  display lastCode
  text " :: "
  display okToPost
  display =<< holdDyn Nothing (_xhrResponse_body <$> r)


lookupID :: String -> [Thrust] -> Maybe UUID.UUID
lookupID tName thrusts =
  let matches s th = map toLower s == map toLower (T.unpack . _thrustName $ th)
  in  _thrustID <$> listToMaybe (filter (matches tName) thrusts)

bothOk :: String -> Maybe UUID.UUID -> Bool
bothOk nm mayID = not (null nm || mayID == Nothing)

toPost :: String -> (String, [Thrust]) -> Maybe XhrRequest
toPost nm (thString, thrusts) = case lookupID thString thrusts of
  Nothing  -> Nothing
  Just pID -> case null nm of
    True  -> Nothing
    False -> let newPI = InsPI (T.pack nm) pID Nothing
                 payload = BL.unpack . A.encode $ newPI
             in Just $ xhrRequest "POST" "/pis"
                (def { _xhrRequestConfig_sendData = Just payload
                     , _xhrRequestConfig_headers = "Content-Type" =: "application/json"})

{-
          toReq nm (Just tID) = let newPI = InsPI (T.pack nm) tID Nothing
                                in xhrRequest "POST" "/pis"
                                   (def {_xhrRequestConfig_sendData = Just . show . A.encode $ newPI})
          toReq _  Nothing    = xhrRequest "POST" "/pis" def
-}
--crudTableWidget
--  :: (MonadWidget t m)
--  => 
