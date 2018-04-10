{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Either
import Data.List hiding (insert)
import Data.Time.Clock
import Database.MongoDB ((=:))
import Database.MongoDB
import Network
import System.Exit
import System.IO
import Text.Parsec
import Text.Printf

import Config
import IrcCommand
import Parser

type Net = ReaderT Bot IO

data Bot = Bot
  { socket :: Handle
  , config :: Config
  , pipe   :: Pipe
  }


main :: IO ()
main = bracket connectBot disconnect loop
  where
    disconnect = hClose . socket >> close . pipe
    loop st = runReaderT run st

connectBot :: IO Bot
connectBot =
  notify $ do
    c <- readConfig
    h <- connectTo (server c) (PortNumber . fromIntegral . port $ c)
    p <- connect (host "127.0.0.1")
    hSetBuffering h NoBuffering
    hSetEncoding h utf8
    return $ Bot h c p
  where
    server   = scAddress . cServer
    port     = scPort    . cServer
    notify a =
      bracket_ (printf "Connecting..." >> hFlush stdout) (putStrLn "done.") a

getMessage :: String -> IrcMessage
getMessage s =
  let message l =
        case (parse parseMessage "ircMessage" l) of
          Left _  -> error "Failed to parse"
          Right x -> x
  in message s

run :: Net ()
run = do
  c <- asks config
  h <- asks socket
  p <- asks pipe
  writeNick $ nick c
  writeUser $ user c
  forM_ (channels c) joinChan
  listen h p
  where
    writeNick = write "NICK"
    writeUser = write "USER"
    joinChan  = write "JOIN"
    nick      = cNickname
    user c    = nick c ++ " 0 * :" ++ nick c
    channels  = scChannels . cServer

makeDocument :: UTCTime -> IrcMessage -> Document
makeDocument t m = [ "time" =: t
                   , "message" =: message ]
  where
    message = case params of
      Right m -> m
      Left  _ -> ""
    params = makeParameters (getParameters m)

insertMessage :: Pipe -> Document -> Net Value
insertMessage p m = do
  e <- access p master "lor" (insert "messages" m)
  return e

listen :: Handle -> Pipe -> Net ()
listen h p =
  forever $ do
    s        <- init `fmap` io (hGetLine h)
    time     <- io $ getCurrentTime
    message  <- pure $ getMessage s
    document <- pure $ makeDocument time message
    e        <- insertMessage p document
    
    io . putStrLn $ s
    io . putStrLn . show $ message
    io . putStrLn . show $ document
    io . print $ show e

    if ping s
      then pong s
      -- else insertMessage p document
      else return ()
  where
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
    forever x = x >> forever x

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
