module Shared where

import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import           Network.Discord
import           Pipes

type DiscordFunction = Message -> Effect DiscordM ()

reply :: Message -> T.Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

edit :: Message -> T.Text -> Effect DiscordM ()
edit msg cont = fetch' $ EditMessage msg cont Nothing

editEmbed :: Message -> T.Text -> Embed -> Effect DiscordM ()
editEmbed msg cont embed = fetch' $ EditMessage msg cont (Just embed)

upload :: Message -> FilePath -> Effect DiscordM ()
upload Message{messageChannel=chan} path = do
  data' <- liftIO $ B.readFile path
  fetch' $ UploadFile chan path data'
