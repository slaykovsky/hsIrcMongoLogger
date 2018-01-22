{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import           Data.Maybe
import           Data.Time.Clock
import           Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Network.SimpleIRC
import           Network.SimpleIRC.Messages
import           Database.MongoDB ((=:))
import qualified Database.MongoDB as M
import           Control.Monad.Trans (liftIO)

import Config

makeMessage :: IrcMessage -> IO [M.Field]
makeMessage m = do
    time <- getCurrentTime

    return $ [ "nickname" =: nickname
             , "target"   =: target
             , "msg"      =: msg
             , "host"     =: host
             , "time"     =: time
             ]
    where
      nickname = E.decodeUtf8 . fromJust $ mNick m
      msg      = E.decodeUtf8 $ mMsg m
      target   = E.decodeUtf8 . fromJust $ mChan m
      host     = E.decodeUtf8 . fromJust $ mHost m

insertMessage :: IrcMessage -> M.Action IO ()
insertMessage m = do
  message <- liftIO $ makeMessage m
  M.insert_ "message" message

run m = do
  insertMessage m

onMessage :: EventFunc
onMessage s m = do
  pipe <- M.connect $ M.host "127.0.0.1"
  e <- M.access pipe M.master "irc" (run m)
  M.close pipe
  TIO.putStrLn . E.decodeUtf8 $ mMsg m

events = [(Privmsg onMessage)]

main :: IO (Either IOError MIrc)
main = do
  config <- readConfig
  let serv = (mkDefaultConfig (scAddress . cServer $ config) (cNickname config))
             { cChannels = scChannels . cServer $ config
             , cEvents   = events
             }
  connect serv False True
