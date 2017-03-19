{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import qualified Data.Text       as T
import           Network.Discord
import           Pipes

import           Token

prefix :: T.Text
prefix = "|>"

reply :: Message -> T.Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

checkPrefix :: T.Text -> Bool
checkPrefix = T.isPrefixOf prefix

getArgs :: T.Text -> [T.Text]
getArgs line = maybeWords noprefix
  where noprefix = T.stripPrefix prefix line
        maybeWords (Just a) = (T.words a)
        maybeWords Nothing  = []

messageHandler :: [T.Text] -> Message -> Effect DiscordM ()
messageHandler ("test":args) msg = reply msg $ T.pack "Hai " `T.append` T.unwords args
messageHandler _ _               = return ()

main :: IO ()
main = runBot (Client bot_token) $ do
  with MessageCreateEvent $ \msg@Message{..} -> do
    let (command:args) = getArgs messageContent
    when (checkPrefix messageContent) $ messageHandler (getArgs messageContent) msg
