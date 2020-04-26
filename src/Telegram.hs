{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram (handleUpdate) where

import qualified Data.Text as T
import Data.Maybe
import Data.Function
import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Web.Telegram.API.Bot
import Text.Show.Pretty
import qualified Data.Map as M
-- import Text.Printf

import Umklapp

iJoined :: Message -> Bool
iJoined m =
  any (\u -> user_username u == Just "UmklappBot") $
    toList (new_chat_member m) ++
    concat (toList (new_chat_members m))

newStory :: User -> Story
newStory user = Story
  { title = ""
  , startedBy = user
  , activeUsers = []
  , nextUser = Nothing
  , privateChats = M.empty
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

setNoStory :: State -> State
setNoStory s = s { currentStory = Nothing }

setCurrentStory :: Story -> State -> State
setCurrentStory story s = s { currentStory = Just story }

rememberPrivateChat :: User -> Chat -> Story -> Story
rememberPrivateChat u c story = story { privateChats = M.insert (user_id u) c (privateChats story) }

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
    { emt_reply_markup = Just (InlineKeyboardMarkup joinKeyboard) })
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


joinKeyboard :: [[InlineKeyboardButton]]
joinKeyboard = [
    [ (inlineKeyboardButton "Ich!") { ikb_callback_data = Just "join" }
    , (inlineKeyboardButton "Ich nicht.") { ikb_callback_data = Just "unjoin" }
    ]
  ]

askNextPlayer :: State -> TelegramClient ()
askNextPlayer s
  | Just story <- currentStory s
  , Just u <- nextUser story
  , Just private_chat <- M.lookup (user_id u) (privateChats story)
  = do
    let c = ChatId (chat_id private_chat)
    void $ sendMessageM $ sendMessageRequest c $
      if null (sentences story)
      then "Du fängst an! Wie lautet der erste Satz der Geschichte?"
      else
        let l = last (sentences story) in
        "Du bist dran. Der letzte Satz von " <> user_first_name (author l) <> " war:\n\n" <>
         phrase l <> "\n\n" <>
         "Wie soll der nächste Satz lauten?\n" <>
         "Wenn er mit „Ende.“ endet, beendet er die Geschichte."

  | otherwise = return ()

endStory :: User -> Story -> TelegramClient ()
endStory user story = do
  void $ sendMessageM $ sendMessageRequest (newMsgChat story) $
    user_first_name user <> " hat die Geschichte beendet. Hier ist sie " <>
    "in voller Pracht:\n" <>
    "\n" <>
    T.unlines (map phrase (sentences story)) <>
    "\n" <>
    "Lust auf nochmal? Schreib einfach /neu!"

userJoins :: State -> User -> Story -> TelegramClient (Maybe State)
userJoins s user story = do
  let story' =
       pickNextPlayer $
       makeActive user $
       story
  updateIntroMessage story'
  let s' = setCurrentStory story' s
  askNextPlayer s'
  return $ Just s'


handleUpdate :: State -> Update -> TelegramClient (Maybe State)
handleUpdate s Update{ message = Just m } = do
  let c = ChatId (chat_id (chat m))
  let send = void . sendMessageM . sendMessageRequest c
  let user = fromJust (from m)
  if
    | Just _txt <- text m
    , Nothing <- from m -> do
      send "Sorry, I am confused. Who are you?"
      return Nothing

    | Just txt <- text m
    , Just story <- currentStory s
    , "/start join" `T.isPrefixOf` txt
    , Private <- chat_type (chat m)
    -> do
        send "Schön dass du dabei bist!"
        userJoins s user $ rememberPrivateChat user (chat m) story

    | Just txt <- text m
    , "/start" `T.isPrefixOf` txt
    , Private <- chat_type (chat m) -> do
      liftIO $ pPrint (txt, user)
      send "Hallo! Lade mich doch in eine Gruppe ein!"
      return Nothing

    | Just txt <- text m
    , Just story <- currentStory s
    , Group <- chat_type (chat m)
    , "/end" `T.isPrefixOf` txt -> do
      endStory user story
      return $ Just $ setNoStory s

    | iJoined m -> do
      send $
        "Hallo! Ich bin der UmklappBot, mit mir könnt ihr das Umklappspiel spielen." <>
        "Dabei schreiben mehrere reihum an einer Geschichte, Satz für Satz, und sehen " <>
        "dabei stets nur den vorherigen Satz. Probiere es doch einfach aus! Mit /neu " <>
        "startest du eine neue Geschichte."
      return Nothing

    | Just txt <- text m
    , "/neu" `T.isPrefixOf` txt
    , Group <- chat_type (chat m)
    , Nothing <- currentStory s
    -> do
      -- TODO: Check if in a group
      let story = newStory user
      r <- sendMessageM $ (sendMessageRequest c (introMessage story))
        { message_reply_markup = Just (ReplyInlineKeyboardMarkup joinKeyboard) }
      let story' = story { newMsgChat = c, newMsg = message_id (result r) }
      return $ Just $ setCurrentStory story' s

    | Just txt <- text m
    , "/neu" `T.isPrefixOf` txt
    , Group <- chat_type (chat m)
    , Just story <- currentStory s
    -> do
      send $ "Es läuft schon eine Geschichte, macht doch die erstmal fertig. " <>
          maybe "Es fehlen allerdings noch Mitspieler."
            (\nu -> "Als nächstes ist " <> user_first_name nu <> " dran.") (nextUser story)
      return Nothing

    | Just txt <- text m
    , "/weristdran" `T.isPrefixOf` txt
    -> do
      send $ case currentStory s of
        Nothing -> "Es läuft gerade keine Geschichte. Mit /neu startest du eine neue!"
        Just story ->
          case nextUser story of
            Just u -> "Es ist gerade " <> user_first_name u <> " dran. " <>
                      user_first_name u <> ", schau mal im privaten Chat!"
            Nothing -> "Es läuft eine Geschichte, aber es fehlen noch Mitspieler."
      return Nothing

    | Just txt <- text m
    , Just story <- currentStory s
    , Just active_user <- nextUser story
    , user_id active_user == user_id user
    , Private <- chat_type (chat m)
    -> do
      let story' = pickNextPlayer $ addSentence user txt story
      if isEndPhrase txt
      then do
        send $ "Das wars! Die volle Geschichte kannst du im Gruppenchat lesen."
        endStory user story'
        return $ Just $ setNoStory s
      else do
        send $ "Ist notiert! " <>
          maybe
            "Sobald weitere Spieler einsteigen, dürfen die weiterspielen."
            (\nu -> "Als nächstes ist " <> user_first_name nu <> " dran.")
            (nextUser story')
        let s' = setCurrentStory story' s
        askNextPlayer s'
        return $ Just s'

    | Just _ <- text m
    , Private <- chat_type (chat m)
    -> do
      send $ "Schön von dir zu hören. Aber eigentlich erwarte ich von dir gerade keinen Beitrag \129300"
      return Nothing

    | otherwise -> do
      liftIO $ pPrint m
      return Nothing

handleUpdate s Update{ callback_query = Just cb }
    | Just wants_to_join <- cq_data cb >>= isJoinCallback
    , Just story <- currentStory s
    = do
    let user = cq_from cb

    if wants_to_join
    then
        if user_id user `M.notMember` privateChats story
        then do
          void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
              { cq_url = Just "https://t.me/umklappbot?start=join" }
          return Nothing
        else do
          void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
              { cq_text = Just "Schön dass du dabei bist!" }
          userJoins s user story
    else do
      void $ answerCallbackQueryM $ (answerCallbackQueryRequest (cq_id cb))
          { cq_text = Just "Schade, andermal vielleicht." }
      let story' = makeInactive user story
      updateIntroMessage story'
      let s' = setCurrentStory story' s
      askNextPlayer s'
      return $ Just s'
  where
    isJoinCallback :: T.Text -> Maybe Bool
    isJoinCallback "join" = Just True
    isJoinCallback "unjoin" = Just False
    isJoinCallback _ = Nothing



handleUpdate _ u = do
  liftIO $ putStrLn $ "Unhandled message:\n" ++ ppShow u
  return Nothing
