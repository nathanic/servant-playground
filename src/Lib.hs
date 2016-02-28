{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import Control.Concurrent (MVar, newMVar)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Aeson.TH
import Data.List (find)
import Data.Maybe (fromJust)

import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import qualified ChatServer as Chat

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
    :<|> "chat" :> Raw

startApp :: IO ()
startApp = do
    chatState <- newMVar Chat.newServerState
    run 8080 $
        serve api (server chatState)

api :: Proxy API
api = Proxy

server :: MVar Chat.ServerState -> Server API
server chatState =
         getUsers
    :<|> getUser
    :<|> Chat.waiApp chatState

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

