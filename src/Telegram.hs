{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Telegram (handleUpdate) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Servant.Client.Core (ClientError(..), responseStatusCode)
import Network.HTTP.Types.Status (status403)
import Web.Telegram.API.Bot
import Text.Show.Pretty
import qualified Data.UUID as UUID
import Data.UUID.V4
-- import Text.Printf

import Umklapp

iJoined :: Message -> Bool
iJoined m =
  any (\u -> user_username u == Just "UmklappBot")
    (toList (new_chat_member m) ++ concat (toList (new_chat_members m)))
  || (group_chat_created m == Just True)

newStory :: StoryId -> User -> Story
newStory sid user = Story
  { storyId = sid
  , title = ""
  , startedBy = user
  , activeUsers = []
  , nextUser = Nothing
  , newMsgChat = error "newMsg looked at"
  , newMsg = error "newMsg looked at"
  , sentences = []
  }

isEndPhrase :: T.Text -> Bool
isEndPhrase txt = or
  [ "Ende" `T.isSuffixOf` txt
  , "Ende." `T.isSuffixOf` txt
  , "ende" `T.isSuffixOf` txt
  , "ende." `T.isSuffixOf` txt
  ]

makeActive :: User -> Story -> Story
makeActive u story =
    if any (((==) `on` user_id) u) (activeUsers story)
    then story
    else story { activeUsers = activeUsers story ++ [u] }

makeInactive :: User -> Story -> Story
makeInactive u story = story
    { activeUsers = removeUser u (activeUsers story)
    , nextUser =
        if Just (user_id u) == (user_id <$> nextUser story)
        then Nothing
        else nextUser story
    }

addSentence :: User -> T.Text -> Story -> Story
addSentence u t story =
  story
    { sentences = sentences story ++ [Sentence t u]
    , nextUser = Nothing
    }

pickNextPlayer :: Story -> Story
pickNextPlayer story
    | Just _ <- nextUser story = story
    | otherwise = story { nextUser = nextToPlay story }

introMessage :: Story -> T.Text
introMessage story =
  "Ok, lass uns eine neue Geschichte schreiben. Wer ist dabei?\n" <>
  "Du kannst jederzeit ein- und aussteigen. Bisher dabei:\n" <>
  (if null (activeUsers story)
   then "Noch niemand…"
   else T.intercalate ", " (map user_first_name (activeUsers story)))


updateIntroMessage :: Story -> TelegramClient ()
updateIntroMessage story = catchError
  (void $ editMessageTextM $ (editMessageTextRequest (newMsgChat story) (newMsg story) (introMessage story))
    { emt_reply_markup = Just (InlineKeyboardMarkup (joinKeyboard story)) })
  (\r -> liftIO $ putStrLn $ "Ignoring failure updating intro message:\n" ++ show r )

removeUser :: User -> [User] -> [User]
removeUser u = filter (((/=) `on` user_id) u)

nextToPlay :: Story -> Maybe User
nextToPlay story
  | null (sentences story)
  , u : _ <- activeUsers story
  = Just u
  | not (null (sentences story))
  = let (last_author:other_autors) = reverse (map author (sentences story)) in
    let candidates = filter (((/=) `on` user_id) last_author) (activeUsers story) in
    dropUntilSmallest other_autors candidates
  | otherwise
  = Nothing
  where
    dropUntilSmallest :: [User] -> [User] -> Maybe User
    dropUntilSmallest _ [] = Nothing
    dropUntilSmallest [] (u:_) = Just u
    dropUntilSmallest (a:as) us | null us' = Just (head us)
                                | otherwise = dropUntilSmallest as us'
      where us' = removeUser a us


joinKeyboard :: Story -> [[InlineKeyboardButton]]
joinKeyboard story = [
    [ (inlineKeyboardButton "Ich!") { ikb_callback_data = Just $ "join " <> UUID.toText (storyId story) }
    , (inlineKeyboardButton "Ich nicht.") { ikb_callback_data = Just $ "unjoin " <> UUID.toText (storyId story) }
    ]
  ]

-- returns False when user disconnected
sendPrivateMessage :: User -> T.Text -> TelegramClient Bool
sendPrivateMessage u msg =
    catchError (True <$ sendMessageM (sendMessageRequest c msg)) $ \case
        FailureResponse _ res | responseStatusCode res == status403
            -> return $ False
        e -> throwError e
  where
    c = ChatId (fromIntegral (user_id u))



askNextPlayer :: Story -> TelegramClient Story
askNextPlayer story
  | Just u <- nextUser story
  = do
    r <- sendPrivateMessage u $
      if null (sentences story)
      then "Du fängst an! Wie lautet der erste Satz der Geschichte?"
      else
        let l = last (sentences story) in
        "Du bist dran. Der letzte Satz von " <> user_first_name (author l) <> " war:\n\n" <>
         phrase l <> "\n\n" <>
         "Wie soll der nächste Satz lauten?\n" <>
         "Wenn er mit „Ende.“ endet, beendet er die Geschichte."
    case r of
        True -> return story
        False -> userLeaves story u
askNextPlayer story = return story


endStory :: User -> Story -> TelegramClient ()
endStory user story = do
  void $ sendMessageM $ sendMessageRequest (newMsgChat story) $
    user_first_name user <> " hat die Geschichte beendet. Hier ist sie " <>
    "in voller Pracht:\n" <>
    "\n" <>
    T.unlines (map phrase (sentences story)) <>
    "\n" <>
    "Lust auf nochmal? Schreib einfach /neu!"

userJoins :: User -> Story -> TelegramClient Story
userJoins user story = do
  let story' =
       pickNextPlayer $
       makeActive user $
       story
  updateIntroMessage story'
  askNextPlayer story'

userLeaves :: Story -> User -> TelegramClient Story
userLeaves story user = do
    let story' = makeInactive user story
    updateIntroMessage story'
    askNextPlayer story'

-- Find the story to send this update to
dispatchUpdate :: State -> Update -> Maybe StoryId
dispatchUpdate s Update{ message = Just m } | Private <- chat_type (chat m)
    = getFirst $ mconcat $
    [ First (Just sid)
    | Just user <- pure $ from m
    , (sid, story) <- M.toList (stories s)
    , user_id user `elem` (user_id <$> activeUsers story)
    ] <>
    [ First (Just sid)
    | Just _user <- pure $ from m
    , Just txt <- pure $ text m
    , (sid, _story) <- M.toList (stories s)
    , ("/start join " <> UUID.toText sid) `T.isPrefixOf` txt
    ]
dispatchUpdate s Update{ message = Just m } | isGroupMessage m
    = getFirst $ mconcat
    [ First (Just sid)
    | (sid, story) <- M.toList (stories s)
    , c == newMsgChat story
    ]
  where c = ChatId (chat_id (chat m))
dispatchUpdate s Update{ callback_query = Just cb } | Just dat <- cq_data cb
    = getFirst $ mconcat $
    [ First (Just sid)
    | let user = cq_from cb
    , (sid, story) <- M.toList (stories s)
    , user_id user `elem` (user_id <$> activeUsers story)
    ] <>
    [ First (Just sid)
    | (sid, _story) <- M.toList (stories s)
    , UUID.toText sid `T.isSuffixOf` dat
    ]
dispatchUpdate _ _ = Nothing


handleUpdate :: State -> Update -> TelegramClient (Maybe State)
handleUpdate s update = case dispatchUpdate s update of
  Just sid -> do
    let story = stories s M.! sid
    handleStory story update >>= \case
         Nothing -> return $ Just $ s { stories = M.delete sid (stories s) }
         Just story' -> return $ Just $ s { stories = M.insert sid story' (stories s) }
  Nothing -> handleOutOfStory s update

isGroupMessage :: Message -> Bool
isGroupMessage m = case chat_type (chat m) of
    Group -> True
    Supergroup -> True
    Channel -> True
    Private -> False

handleOutOfStory :: State -> Update -> TelegramClient (Maybe State)
handleOutOfStory s Update{ message = Just m }
    | Just _txt <- text m
    , Nothing <- from m
    = do
      send "Sorry, I am confused. Who are you?"
      return Nothing

    | Just txt <- text m
    , "/start" `T.isPrefixOf` txt
    , Private <- chat_type (chat m)
    = do
      liftIO $ pPrint (txt, user)
      send "Hallo! Lade mich doch in eine Gruppe ein, und starte dort ein Spiel!"
      return Nothing

    | Just txt <- text m
    , "/neu" `T.isPrefixOf` txt
    , isGroupMessage m
    = do
      sid <- liftIO nextRandom
      let story = newStory sid user
      r <- sendMessageM $ (sendMessageRequest c (introMessage story))
        { message_reply_markup = Just (ReplyInlineKeyboardMarkup (joinKeyboard story)) }
      let story' = story { newMsgChat = c, newMsg = message_id (result r) }
      return $ Just $ s { stories = M.insert sid story' (stories s) }

    | Just txt <- text m
    , any (`T.isPrefixOf` txt) ["/end", "/nag", "/weristdran"]
    , isGroupMessage m
    = do
      send "Es läuft gerade keine Geschichte. Starte doch eine mit /neu!"
      return Nothing

    | iJoined m
    = do
      send $
        "Hallo! Ich bin der UmklappBot, mit mir könnt ihr das Umklappspiel spielen. " <>
        "Dabei schreiben mehrere reihum an einer Geschichte, Satz für Satz, und sehen " <>
        "dabei stets nur den vorherigen Satz. Probiere es doch einfach aus! Mit /neu " <>
        "startest du eine neue Geschichte."
      return Nothing

    | Just _ <- text m
    , Private <- chat_type (chat m)
    = do
      send "Schön von dir zu hören. Aber eigentlich erwarte ich von dir gerade keinen Beitrag \129300"
      return Nothing
  where
    c = ChatId (chat_id (chat m))
    send = void . sendMessageM . sendMessageRequest c
    user = fromJust (from m)

handleOutOfStory _ Update{ callback_query = Just cb }
    | Just (_wants_to_join, _join_story_id) <- cq_data cb >>= isJoinCallback
    = do
      void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
          { cq_text = Just "Diese Geschichte ist schon beendet." }
      return Nothing

handleOutOfStory _ u = do
  liftIO $ putStrLn $ "Unhandled out-of-story update:\n" ++ ppShow u
  return Nothing


handleStory :: Story -> Update -> TelegramClient (Maybe Story)
handleStory story Update{ message = Just m }
    | Just txt <- text m
    , ("/start join " <> UUID.toText (storyId story)) `T.isPrefixOf` txt
    , Private <- chat_type (chat m)
    = do
      send "Schön dass du dabei bist!"
      Just <$> userJoins user story

    | Just txt <- text m
    , "/start join " `T.isPrefixOf` txt
    , Private <- chat_type (chat m)
    = do
      send "Du schon bei einem anderen Spiel dabei... "
      return (Just story)

    | Just txt <- text m
    , isGroupMessage m
    , "/end" `T.isPrefixOf` txt
    = do
      endStory user story
      return Nothing

    | Just txt <- text m
    , isGroupMessage m
    , "/nag" `T.isPrefixOf` txt
    = case nextUser story of
        Just u -> do
          send $ "Es ist gerade " <> user_first_name u <> " dran, ich nerv mal ein bisschen."
          Just <$> askNextPlayer story
        Nothing -> do
          send "Es läuft eine Geschichte, aber es fehlen noch Mitspieler."
          return (Just story)

    | Just txt <- text m
    , "/neu" `T.isPrefixOf` txt
    , isGroupMessage m
    = do
      send $ "Es läuft schon eine Geschichte, macht doch die erstmal fertig. " <>
          maybe "Es fehlen allerdings noch Mitspieler."
            (\nu -> "Als nächstes ist " <> user_first_name nu <> " dran.") (nextUser story)
      return (Just story)

    | Just txt <- text m
    , "/weristdran" `T.isPrefixOf` txt
    = do
      send $ case nextUser story of
        Just u -> "Es ist gerade " <> user_first_name u <> " dran. " <>
                  user_first_name u <> ", schau mal im privaten Chat!"
        Nothing -> "Es läuft eine Geschichte, aber es fehlen noch Mitspieler."
      return (Just story)

    | Just txt <- text m
    , Just active_user <- nextUser story
    , user_id active_user == user_id user
    , Private <- chat_type (chat m)
    = do
      let story' = pickNextPlayer $ addSentence user txt story
      if isEndPhrase txt
      then do
        send "Das wars! Die volle Geschichte kannst du im Gruppenchat lesen."
        endStory user story'
        return Nothing
      else do
        send $ "Ist notiert! " <>
          maybe
            "Sobald weitere Spieler einsteigen, dürfen die weiterspielen."
            (\nu -> "Als nächstes ist " <> user_first_name nu <> " dran.")
            (nextUser story')
        Just <$> askNextPlayer story'

  where
    c = ChatId (chat_id (chat m))
    send = void . sendMessageM . sendMessageRequest c
    user = fromJust (from m)

handleStory story Update{ callback_query = Just cb }
    | Just (wants_to_join, join_story_id) <- cq_data cb >>= isJoinCallback
    = do
    let user = cq_from cb

    if join_story_id == storyId story
    then if wants_to_join
      then do
        success <- sendPrivateMessage user "Schön dass du dabei bist!"
        if success
        then do
          void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
              { cq_text = Nothing }
          Just <$> userJoins user story
        else do
          void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
              { cq_url = Just $ "https://t.me/umklappbot?start=join " <> UUID.toText (storyId story) }
          return (Just story)
      else do
        void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
            { cq_text = Just "Schade, andermal vielleicht." }
        Just <$> userLeaves story user
    else if wants_to_join
      then do
        void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
            { cq_text = Just "Du schreibst doch schon bei einem anderen Spiel mit!" }
        return (Just story)
      else do
        void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
            { cq_text = Just "Du bist bei dem Spiel doch gar nicht dabei!" }
        return (Just story)

handleStory story u = do
  liftIO $ putStrLn $ "Unhandled in-story update:\n" ++ ppShow u
  return (Just story)

isJoinCallback :: T.Text -> Maybe (Bool, StoryId)
isJoinCallback t
  | Just story_id <- "join " `T.stripPrefix` t >>= UUID.fromText
  = Just (True, story_id)
isJoinCallback t
  | Just story_id <- "unjoin " `T.stripPrefix` t >>= UUID.fromText
  = Just (False, story_id)
isJoinCallback _ = Nothing


deriving instance Eq ChatId
