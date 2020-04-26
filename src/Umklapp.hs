{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Umklapp where

import qualified Data.Text as T
import Web.Telegram.API.Bot (User, Chat, ChatId)
import GHC.Generics
import Data.Aeson.Types
import qualified Data.Map as M

data Story = Story
    { title :: T.Text
    , startedBy :: User
    , activeUsers :: [User]
    , privateChats :: M.Map Int  Chat -- user id to private chat
    , nextUser :: Maybe User
    , sentences :: [Sentence]
    , newMsgChat :: ChatId -- chat and message that started this, to update list of players
    , newMsg :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

data Sentence = Sentence
    { phrase :: T.Text
    , author :: User
    } deriving (Show, Generic, ToJSON, FromJSON)

newtype State = State
    { currentStory :: Maybe Story
    } deriving (Show, Generic, ToJSON, FromJSON)

newState :: State
newState = State Nothing
