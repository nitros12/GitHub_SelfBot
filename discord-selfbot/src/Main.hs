{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Network.Discord

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Text              as T
import           Pipes
import           System.IO.Unsafe

import           Commands
import           Shared
import           Token

prefix :: T.Text
prefix = "|>"

checkPrefix :: T.Text -> Bool
checkPrefix = T.isPrefixOf prefix

cutFirstSpace :: String -> (String, String)
cutFirstSpace = fmap (drop 1) . break (== ' ')

splitCmd :: T.Text -> (String, String)
splitCmd line = unMaybeWords noprefix
  where noprefix = T.stripPrefix prefix line
        unMaybeWords (Just a) = cutFirstSpace $ T.unpack a
        unMaybeWords Nothing  = ("", "")

messageHandler :: (String, String) -> DiscordFunction
messageHandler (command, args) msg = case command of
  "test"    -> testCommand args msg
  "eval"    -> evalHandler args msg
  "emojify" -> emojiFy args msg
  "upload"  -> testUp args msg
  "bf"      -> brainfuck args msg
  "bef"     -> befunge args msg
  _         -> return ()

main :: IO ()
main = runBot (Client bot_token) $ do
  with MessageCreateEvent $ \msg@Message{..} -> do
    when (checkPrefix messageContent && (userId messageAuthor == 184385816066392064)) $ messageHandler (splitCmd messageContent) msg
