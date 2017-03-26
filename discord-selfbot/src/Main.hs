{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Monad
import qualified Data.Text           as T
import           Network.Discord
import           Pipes
import           System.Eval.Haskell
import           System.Exit         (ExitCode (..))
import           System.IO           (hGetContents)
import           System.Process      (runInteractiveCommand, waitForProcess)
import System.IO.Unsafe
import Control.Concurrent.STM


import           Token

type ArgList = [T.Text]
type DiscordFunction = ArgList -> Message -> Effect DiscordM ()

prefix :: T.Text
prefix = "|>"

owner :: TVar Snowflake
owner = unsafePerformIO $ newTVarIO 0

reply :: Message -> T.Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

edit :: Message -> T.Text -> Effect DiscordM ()
edit msg cont = fetch' $ EditMessage msg cont Nothing

editEmbed :: Message -> T.Text -> Embed -> Effect DiscordM ()
editEmbed msg cont embed = fetch' $ EditMessage msg cont (Just embed)

checkPrefix :: T.Text -> Bool
checkPrefix = T.isPrefixOf prefix

getArgs :: T.Text -> ArgList
getArgs line = maybeWords noprefix
  where noprefix = T.stripPrefix prefix line
        maybeWords (Just a) = (T.words a)
        maybeWords Nothing  = []

formatCode :: T.Text -> T.Text
formatCode = fix . (T.stripPrefix "`") . fix . (T.stripSuffix "``")
  where fix (Just a) = a
        fix Nothing  = ""

hsEval :: IO ()
hsEval inp = do i <- eval "1 + 6 :: Int" [] :: IO (Maybe Str)
                when (isJust i) $ putStrLn (show (fromJust i))

testCommand :: DiscordFunction
testCommand args msg = edit msg $ T.pack "Hai " `T.append` T.unwords args


execProgram :: String -> IO (String, ExitCode)
execProgram cmdline = do
  (pin, out, err, handle) <- runInteractiveCommand cmdline
  exitCode <- waitForProcess handle
  output <- hGetContents out
  return (output, exitCode)

hsEvalCommand :: DiscordFunction
hsEvalCommand args msg = edit msg $ (T.pack "Haskell output: \n```") `T.append` (T.pack . hsCmd . T.unpack . formatCode $ T.unwords args) `T.append` "```"


messageHandler :: DiscordFunction
messageHandler (command:args) msg = case command of
  "test"    -> testCommand args msg
  "hs.eval" -> hsEvalCommand args msg
messageHandler _ _                = return ()

main :: IO ()
main = runBot (Client bot_token) $ do
  with ReadyEvent $ \(Init v (User {..}) _ _ _) -> do
    liftIO . atomically $ writeTVar owner userId

  with MessageCreateEvent $ \msg@Message{..} -> do
    me <- liftIO $ readTVarIO owner
    let (command:args) = getArgs messageContent
    when (checkPrefix messageContent && (userId messageAuthor == me) $ messageHandler (getArgs messageContent) msg
