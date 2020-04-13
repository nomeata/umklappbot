{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Concurrent (threadDelay)
import Aws.Lambda
import GHC.Generics
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Functor.Identity
import System.Environment
import Web.Telegram.API.Bot.API
import Network.HTTP.Client      (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import qualified Aws
import qualified Aws.DynamoDb as D
import Control.Monad.Trans.Resource
import qualified Web.Telegram.API.Bot as Tgl

import Telegram
import Umklapp

type Settings = (Token, Manager, Aws.Configuration, D.DdbConfiguration Aws.NormalQuery)

runT :: Settings -> TelegramClient a -> IO a
runT (token, manager, _, _) act =
    either (error . show) id <$>
        runTelegramClient token manager act

getSettings :: IO Settings
getSettings = do
  token <- getEnv "TELEGRAM_TOKEN"
  let t = Token ("bot" <> T.pack token)
  manager <- newManager tlsManagerSettings

  aws_cfg <- Aws.baseConfiguration
  let dcfg = (def :: D.DdbConfiguration Aws.NormalQuery ) { D.ddbcRegion = D.ddbEuCentral1 }

  return (t, manager, aws_cfg, dcfg)

getState :: Settings -> IO State
getState (_, mgr, cfg, dcfg) = do
  r <- runResourceT $ Aws.aws cfg dcfg mgr $
     D.getItem "umklappbot" (D.hk "id" "state")
  return $ either (error . show) (runIdentity. ($ return) . ($ error) . D.runParser . D.getAttr "state" . fromMaybe undefined . D.girItem) $ Aws.responseResult r

putState :: Settings -> State -> IO ()
putState (_, mgr, cfg, dcfg) s = do
  r <- runResourceT $ Aws.aws cfg dcfg mgr $
     D.putItem "umklappbot" $
     D.item [ D.attr @T.Text "id" "state"
            , D.attr "state" s
            ]
  return $ either (error . show) (const ()) $ Aws.responseResult r

isAmazonLambda :: IO Bool
isAmazonLambda =
    isJust <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"

main :: IO ()
main = do
    settings <- getSettings
    isAmazonLambda >>= \case
        True -> runLambda (handleLambda settings)
        False -> mainLocal settings

handleLambda :: Settings -> LambdaOptions -> IO (Either String LambdaResult)
handleLambda settings opts = do
    result <- handler settings (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
    either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result

mainLocal :: Settings -> IO ()
mainLocal settings = runT settings $ poll settings Nothing

poll :: Settings -> Maybe Int -> TelegramClient ()
poll settings offset = do
  liftIO $ putStrLn "Polling"
  updates <- catchError (Tgl.result <$> Tgl.getUpdatesM Tgl.getUpdatesRequest
    { Tgl.updates_offset = offset
    , Tgl.updates_allowed_updates = Just []
    , Tgl.updates_limit = Just 1
    , Tgl.updates_timeout = Just 10
    }) (\r -> liftIO $ do
       print r
       threadDelay (10*1000*1000)
       return []
    )
  if null updates
  then do
    liftIO $ putStrLn "No message"
    poll settings offset
  else do
    liftIO $ putStrLn "Got a message"
    let u = head updates
    s <- liftIO $ getState settings
    catchError (handleUpdate s u >>= maybe (return ()) (liftIO . putState settings))
        (liftIO . print)
    poll settings (Just (Tgl.update_id u + 1))



deriving instance Show LambdaOptions

data Event = Event
  { path :: T.Text
  , body :: Maybe T.Text
  } deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , headers :: Value
    , body :: T.Text
    , isBase64Encoded :: Bool
    } deriving (Generic, ToJSON)

plainResponse :: T.Text -> IO (Either String Response)
plainResponse txt =
    pure $ Right Response
        { statusCode = 200
        , headers = object [
            "Content-Type" .= ("text/plain" :: String)
        ]
        , isBase64Encoded = False
        , body = txt
        }

telegramPath :: Settings -> T.Text
telegramPath (Token token, _, _, _) = "/telegram/" <> token

handler :: Settings -> Event -> Context -> IO (Either String Response)
handler settings Event{body, path} _context
    | path == telegramPath settings =
        case eitherDecode (LBS.fromStrict (T.encodeUtf8 (fromMaybe "" body))) of
            Left err -> pure $ Right Response
                { statusCode = 400
                , headers = object [
                    "Content-Type" .= ("text/plain" :: String)
                ]
                , isBase64Encoded = False
                , body = "Could not decode message: " <> T.pack err
                }
            Right update -> do
                s <- getState settings
                mbs <- runT settings $ handleUpdate s update
                for_ mbs $ \s' -> putState settings s'
                pure $ Right Response
                    { statusCode = 200
                    , headers = object [
                        "Content-Type" .= ("text/plain" :: String)
                    ]
                    , isBase64Encoded = False
                    , body = "Done"
                    }

    | path == "/init" = do
        cfg <- Aws.baseConfiguration
        let dcfg = (def :: D.DdbConfiguration Aws.NormalQuery ) { D.ddbcRegion = D.ddbEuCentral1 }
        mgr <- newManager tlsManagerSettings
        r <- runResourceT $ Aws.aws cfg dcfg mgr $
            (D.putItem "umklappbot" $
               D.item
                [ D.attr @T.Text "id" "state"
                , D.attr "state" newState
                ]
            )
            { D.piExpect = D.Conditions D.CondAnd [
                 D.Condition "id" D.IsNull
              ]
            }
        plainResponse $ T.pack $ show (Aws.responseResult r)

    | path == "/dump" = do
        cfg <- Aws.baseConfiguration
        let dcfg = (def :: D.DdbConfiguration Aws.NormalQuery ) { D.ddbcRegion = D.ddbEuCentral1 }
        mgr <- newManager tlsManagerSettings
        r <- runResourceT $ Aws.aws cfg dcfg mgr $ D.DescribeTable "umklappbot"
        plainResponse $ T.pack $ show (Aws.responseResult r)


    | otherwise = plainResponse $ "Nothing to see here at " <> path

-- Horrible way of using DyanamoDB, storing stuff as JSON.
-- Is there a better way?
instance D.DynVal State where
    type DynRep State = D.DynString
    fromRep = either error id . eitherDecode . LBS.fromStrict . T.encodeUtf8 . D.unDynString
    toRep = D.DynString . T.decodeUtf8 . LBS.toStrict . encode

