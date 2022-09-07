module Main where

import qualified Data.Text                        as Text
import           Data.Maybe

import Telegram.Bot.API
    ( defaultTelegramClientEnv,
      Token,
      Update,
      extractUpdateMessage,
      Message(messageText),
      MessageId,
      Message(messageText), messageMessageId )
import Telegram.Bot.Simple
    ( getEnvToken, startBot_, (<#), Eff, BotApp(..) )
import Telegram.Bot.Simple.Debug (traceBotDefault, traceBotActionsShow, traceTelegramUpdatesJSON, ppAsJSON)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, plainText, updateMessageText, mkParser, UpdateParser)
import Telegram.Bot.API.Types (messageText)
import Control.Monad.Cont ( MonadIO(liftIO) )
import Control.Applicative (Applicative(liftA2))
import Data.HashMap.Strict (HashMap, empty, insert, delete)

newtype Model = Model {
  modelMap :: HashMap MessageId Text.Text
}
  deriving Show

initialModel :: Model
initialModel = Model empty

add :: MessageId -> Text.Text -> Model -> Model
add msgId msgText model = Model 
  $ insert msgId msgText 
  $ modelMap model

delete_ :: MessageId -> Model -> Model
delete_ msgId model = Model
  $ delete msgId
  $ modelMap model

data Action = NoAction | Add MessageId Text.Text | Delete MessageId
    deriving Show

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = initialModel
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ update = flip parseUpdate update $ do
  msgId    <- getMessageId
  msgText  <- getMessageText
  pure $ if msgText == Text.empty then Delete msgId else Add msgId msgText

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction          -> pure model
  Add msgId msgText -> add msgId msgText model <# do
    pure NoAction
  Delete msgId      -> delete_ msgId model <# do
    pure NoAction
  

messageParser :: UpdateParser Message
messageParser = mkParser extractUpdateMessage

getMessageText :: UpdateParser Text.Text
getMessageText = fromJust . messageText <$> messageParser

getMessageId :: UpdateParser MessageId
getMessageId = messageMessageId <$> messageParser


run :: IO ()
run = do
  t <- token
  env <- defaultTelegramClientEnv t
  startBot_  (traceBotDefault bot) env

token :: IO Token
token = getEnvToken "TELEGRAM_BOT_TOKEN"

main :: IO ()
main = run
