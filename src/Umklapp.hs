{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Umklapp where

import qualified Data.Text as T
import Web.Telegram.API.Bot (User, ChatId)
import GHC.Generics
import Data.Aeson.Types
import Data.UUID
import qualified Data.Map as M

data Story = Story
    { storyId :: StoryId
    , title :: T.Text
    , startedBy :: User
    , activeUsers :: [User]
    , nextUser :: Maybe User
    , sentences :: [Sentence]
    , newMsgChat :: ChatId -- chat and message that started this, to update list of players
    , newMsg :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

data Sentence = Sentence
    { phrase :: T.Text
    , author :: User
    } deriving (Show, Generic, ToJSON, FromJSON)


-- Just using a list of stories, and then always searching
-- for the right story given a user or chat,
-- is of course not efficient. But lets get functionality first right.
-- Maybe data base queries can be used instead of maintaining our own indices
type StoryId = UUID
newtype State = State
    { stories :: M.Map StoryId Story
    } deriving (Show, Generic, ToJSON, FromJSON)

newState :: State
newState = State mempty
