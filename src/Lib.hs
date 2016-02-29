{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Concurrent (MVar, newMVar, modifyMVar)
import Control.Monad (join)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Aeson.TH
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import qualified ChatServer as Chat

type AppState = MVar (Map String (MVar Chat.ServerState))

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
    :<|> "chat" :> Capture "chatroom" String
                :> Raw

startApp :: IO ()
startApp = do
    chatStates <- newMVar mempty
    run 8080 $
        serve api (server chatStates)

api :: Proxy API
api = Proxy

server :: AppState -> Server API
server chatStates =
         getUsers
    :<|> getUser
    :<|> getChatroom chatStates

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Agent" "Einstein"
        , User 4 "Fig" "Newton"
        ]

getUsers mfirst mlast = return $ filter (\u -> makePred mfirst userFirstName u
                                               && makePred mlast userLastName u)
                                        users
  where makePred needle field = maybe (const True) (\s -> (== s) . field) needle

getUser uid = case find ((== uid) . userId) users of
    Just u -> return u
    Nothing -> left err404 { errBody = "Dave's not here, man." }

getChatroom :: AppState -> String -> Application
getChatroom chatStates chatroom req respond = do
    state <- modifyMVar chatStates $ \states ->
        case Map.lookup chatroom states of
            Just state -> return (states, state)
            Nothing    -> do
                state <- newMVar Chat.newServerState
                return (Map.insert chatroom state states, state)
    Chat.waiApp state req respond

