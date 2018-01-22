{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Data.List
import Data.Either
import Network
import Prelude hiding (putStrLn, drop, init, concat)
import System.IO hiding (putStrLn, hGetLine)
import Data.Text
import Data.Text.IO (putStrLn, hGetLine)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Text.Parsec

import Parser

server  = "irc.freenode.org"
port    = 6667
channel = "#lor"
nick    = "lorCombinator"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

main :: IO ()
main = bracket connect disconnect loop
    where
      disconnect = hClose . socket
      loop st    = runReaderT run st

connect :: IO Bot
connect = notify $ do
            h <- connectTo server (PortNumber (fromIntegral port))
            hSetBuffering h NoBuffering
            hSetEncoding h utf8
            return (Bot h)
    where
      notify a = bracket_
                   (printf "Connecting to %s ..." server >> hFlush stdout)
                   (putStrLn "done.")
                   a

getMessage :: Text -> Net IrcMessage
getMessage s =
    let
        message l = case (parse parseMessage "ircMessage" l) of
                    Left _ -> error "Failed to parse"
                    Right x -> x
  in do
    return $ message (unpack s)

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (concat [nick, " 0 * :lor Combinator"])
  write "JOIN" channel
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
             s <- init `fmap` io (hGetLine h)
             message <- getMessage s
             io . putStrLn $ s
             io . putStrLn . pack . show $ message
             if ping s then pong s else return ()
    where
      ping x = "PING :" `isPrefixOf` x
      pong x = write "PONG" (concat [singleton ':', drop 6 x])


privmsg :: Text -> Net ()
privmsg s = write "PRIVMSG" (concat [channel, " :", s])

write :: Text -> Text -> Net()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
