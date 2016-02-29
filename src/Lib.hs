{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( startApp
    ) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Aeson.TH
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import qualified ChatServer as Chat

data State = State { chatStates :: Map String (MVar Chat.ServerState)
                   , users :: [User]
                   }

type AppState = MVar State

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> QueryParam "firstName" String
                   :> QueryParam "lastName" String
                   :> Get '[JSON] [User]
    :<|> "user" :> Capture "userid" Int
                :> Get '[JSON] User
    :<|> "users" :> ReqBody '[JSON] User
                 :> Post '[JSON] User
    :<|> "chat" :> Capture "chatroom" String
                :> Raw

startApp :: IO ()
startApp = do
    stateRef <- newMVar $ State mempty initialUsers
    run 8080 $
        serve api (server stateRef)

api :: Proxy API
api = Proxy

-- TODO: probably switch to embedding this crap in a monad
server :: AppState -> Server API
server stateRef =
         getUsers stateRef
    :<|> getUser stateRef
    :<|> makeUser stateRef
    :<|> getChatroom stateRef

initialUsers :: [User]
initialUsers =
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    , User 3 "Agent" "Einstein"
    , User 4 "Fig" "Newton"
    ]

getUsers stateRef mfirst mlast = do
    State{..} <- liftIO $ readMVar stateRef
    return $ filter (\u -> makePred mfirst userFirstName u
                           && makePred mlast userLastName u)
                    users
  where makePred needle field = maybe (const True) (\s -> (== s) . field) needle

getUser stateRef uid = do
    State{..} <- liftIO $ readMVar stateRef
    case find ((== uid) . userId) users of
        Just u  -> return u
        Nothing -> left err404 { errBody = "Dave's not here, man." }

makeUser stateRef user = do
    liftIO $ modifyMVar_ stateRef $
        \st@State{..} -> return st { users = user : users }
    return user

-- I'm exploiting the fact that Application is an alias for
--   Request → (Response → IO ResponseReceived) → IO ResponseReceived
-- grabbing its params so that I can steal its monad for my MVar effects
getChatroom :: AppState -> String -> Application
getChatroom stateRef chatroom req respond = do
    State{..} <- readMVar stateRef
    roomState <- modifyMVar stateRef $ \st@State{..} ->
        case Map.lookup chatroom chatStates of
            Just roomState -> return (st, roomState)
            Nothing        -> do
                roomState <- newMVar Chat.newServerState
                let chatStates' = Map.insert chatroom roomState chatStates
                return (st { chatStates = chatStates'}, roomState)
    Chat.waiApp roomState req respond


