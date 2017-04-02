{-# LANGUAGE OverloadedStrings #-}
module Commands where

import           Network.Discord

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Pipes
import           System.Exit            (ExitCode (..))
import           System.Process

import           Shared

numMap :: Map Char String
numMap = Map.fromList [('0', ":zero:"),
                        ('1', ":one:"),
                        ('2', ":two:"),
                        ('3', ":three:"),
                        ('4', ":four:"),
                        ('5', ":five:"),
                        ('6', ":six:"),
                        ('7', ":seven:"),
                        ('8', ":eight:"),
                        ('9', ":nine:")]

{--Modification of https://gitlab.com/jkoike/lambdacord/blob/5da11325456925dd1a49ec28e0567bdb8619676f/src/Main.hs#L29--}
codeblock :: Parser (Text, Text)
codeblock = do
      skipWhile (/= '`')
      string "```"
      lang <- takeTill (== '\n')
      char '\n'
      block <- takeTill (== '`')
      string "```"
      return $ (lang, block)

hseval :: String -> IO String
hseval code = do
  (exit, stdout, stderr) <- readProcessWithExitCode "hseval" [code] ""
  if exit == ExitSuccess
    then return stdout
    else return stderr

pyeval :: String -> IO String
pyeval code = do
  (exit, stdout, stderr) <- readProcessWithExitCode "python" ["pyeval.py", code] ""
  if exit == ExitSuccess
    then return stdout
    else return stderr

testCommand :: String -> DiscordFunction
testCommand rest msg = edit msg $ T.pack ("Hai " ++ rest)

evalHandler :: String -> DiscordFunction
evalHandler rest msg = do
  let (typ, code) = fromMaybe ("", "") (maybeResult $ parse codeblock (T.pack rest))
  res <- case typ of
    "haskell" -> (liftIO . hseval $ T.unpack code)
    "hs"      -> (liftIO . hseval $ T.unpack code)
    "python"  -> (liftIO . pyeval $ T.unpack code)
    "py"      -> (liftIO . pyeval $ T.unpack code)
    _         -> return "Cannot eval this code"
  reply msg $ T.pack ("```" ++ (T.unpack typ) ++ "\n" ++ res ++ "```")

getEmoji :: Char -> String
getEmoji char
  | char `elem` map (head . show) [0..9] = numMap Map.! char
  | (toLower char) `elem` ['a'..'z'] = ":regional_indicator_" ++ [toLower char] ++ ":"
  | otherwise = " "

emojiFy :: String -> DiscordFunction
emojiFy rest msg = edit msg $ T.pack (concatMap getEmoji rest)

testUp :: String -> DiscordFunction
testUp rest msg = upload msg rest
