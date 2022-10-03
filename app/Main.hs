{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text                        as Text
import           Data.Maybe

import Telegram.Bot.API
    ( defaultTelegramClientEnv,
      Token,
      Update,
      extractUpdateMessage,
      Message(messageText, messageSenderChat, messageNewChatTitle),
      MessageId,
      Message(messageText), messageMessageId, deleteMessage, Response (Response, responseResult), updateChatId )
import Telegram.Bot.Simple
    ( getEnvToken, startBot_, (<#), Eff, BotApp(..) )
import Telegram.Bot.Simple.Debug (traceBotDefault, traceBotActionsShow, traceTelegramUpdatesJSON, ppAsJSON)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, plainText, updateMessageText, mkParser, UpdateParser)
import Telegram.Bot.API.Types (messageText, ChatId, chatId)
import Control.Monad.Cont ( MonadIO(liftIO) )
import Control.Applicative (Applicative(liftA2), (<|>))
import Data.HashMap.Strict (HashMap, empty, insert, delete, filterWithKey, mapMaybeWithKey)
import Servant.Client.Internal.HttpClient (ClientM (ClientM))

newtype Model = Model {
  modelMap :: HashMap MessageId Message
}
  deriving Show

initialModel :: Model
initialModel = Model empty

add :: MessageId -> Message -> Model -> Model
add msgId msg model = Model
  $ insert msgId msg
  $ modelMap model

-- delete_ :: ChatId -> MessageId -> Model -> Model
-- delete_ chatId_ msgId model = Model
  -- $ mapMaybeWithKey f
  -- $ modelMap model
  -- where
  --   f k v = do
  --     isDeleted <- responseResult <$> deleteMessage chatId_ k
  --     if isDeleted then
  --       Just v
  --     else
  --       Nothing

data Action = NoAction
    | Add MessageId Message
    -- | Delete ChatId MessageId
    deriving Show

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = initialModel
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate model = parseUpdate $ do
  msgId    <- getMessageId
  -- chatId_   <- getChatId
  Add msgId <$> messageParser

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction             -> pure model
  Add msgId msg    -> add msgId msg model <# do
    pure NoAction
  -- Delete chatId_ msgId -> delete_ chatId_ msgId model <# do
  --   pure NoAction


messageParser :: UpdateParser Message
messageParser = mkParser extractUpdateMessage

getChatId :: UpdateParser ChatId
getChatId = mkParser updateChatId

getMessageText :: UpdateParser Text.Text
getMessageText = fromJust . messageText <$> messageParser

getMessageId :: UpdateParser MessageId
getMessageId = messageMessageId <$> messageParser

-- getChatId :: UpdateParser ChatId
-- getChatId =  chatId . fromJust . messageSenderChat <$> messageParser


run :: IO ()
run = do
  t <- token
  env <- defaultTelegramClientEnv t
  startBot_  (traceBotDefault bot) env

token :: IO Token
token = getEnvToken "TELEGRAM_BOT_TOKEN"

main :: IO ()
main = run
