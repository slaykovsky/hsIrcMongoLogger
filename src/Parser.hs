{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (IrcMessage(..), parseMessage) where

import Prelude hiding (concat)
import Data.Foldable hiding (concat)
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Text.Lazy (Text, concat, intercalate, pack, singleton, splitOn)

import IrcCommand
import CommandsTable

privmsg = ":Macha!~macha@unaffiliated/macha PRIVMSG #botwar :Test response"
notice = ":card.freenode.net NOTICE * :*** Looking up your hostname..."
welcome = ":card.freenode.net 001 lorCombinator :Welcome to the freenode Internet Relay Chat Network lorCombinator"
yourHost = ":wolfe.freenode.net 002 lorCombinator :Your host is wolfe.freenode.net[193.10.255.100/6667], running version ircd-seven-1.1.4"
created = ":verne.freenode.net 003 lorCombinator :This server was created Sat Jan 7 2017 at 11:53:54 EST"
chanurl = ":services. 328 lorCombinator #lor :https://linux.org.ru"

data IrcPrefix = IrcPrefixHost IrcHostName
               | IrcPrefixUser IrcUser
               | IrcPrefixDefault
                 deriving (Show)

data IrcUser = IrcUser
    { nickName :: Maybe IrcNickName
    , userName :: Maybe IrcUserName
    , hostName :: Maybe IrcHostName
    , server   :: Maybe IrcHostName
    } deriving (Show)

data IrcNickName = IrcNickName Text deriving (Show)
data IrcUserName = IrcUserName Text deriving (Show)
data IrcHostName = IrcHostName Text deriving (Show)

data IrcMessage = IrcMessage {
      iPrefix     :: IrcPrefix
    , iCommand    :: IrcCommand
    , iParameters :: Maybe IrcParameters
} deriving (Show)

data IrcParameters = IrcParamMsg { targets :: [IrcTarget], message :: Text }
                   | IrcParamChan { channels :: [IrcTarget], keys :: Maybe [Text] }
                   | IrcParamUser { username :: Text, mode :: Int, realname :: Text }
                   | IrcParamDefault [Text]
                     deriving (Show)

data IrcTarget = IrcGlobalChannel Text
               | IrcLocalChannel Text
               | IrcTargetUser IrcUser
               | IrcTargetUnknown Text
                 deriving (Show)

makeCommand :: IrcCommand -> Either Text Text
makeCommand c = case c of
                  CmdNotice  -> Right "NOTICE"
                  CmdPrivmsg -> Right "PRIVMSG"
                  CmdPong    -> Right "PONG"
                  _          -> Left $ concat ["Unknown command: ", (pack $ show c)]

makeParameters :: Maybe IrcParameters -> Either Text Text
makeParameters p = case p of
                     Just (IrcParamMsg ts msg) -> Right $ ircParamMsg ts msg
                     Just (IrcParamChan cs ks) -> Right $ ircParamChan cs ks
                     Just (IrcParamUser u m r) -> Right $ ircParamUser u m r
                     Just (IrcParamDefault ps) -> Right $ intercalate " :" ps
                     Nothing -> Right ""
    where
      ircParamUser u m r = intercalate " " [u, (pack . show $ m), "*", concat [":", r]]
      ircParamMsg ts msg = concat [targetsMerge ts, " :", msg]
      ircParamChan cs ks = case (cs, ks) of
                             (cs, Nothing) -> targetsMerge cs
                             (cs, Just ks) -> concat[targetsMerge cs, " ", commaConcat ks]
      targetsMerge ts = commaConcat $ map getTarget ts
      commaConcat = intercalate ","
      getTarget t = case t of
                      IrcLocalChannel c  -> concat ["&", c]
                      IrcGlobalChannel c -> concat ["#", c]

parseTarget :: Parser IrcTarget
parseTarget = try getUnknownTarget
              <|> try parseChannel
              <|> try getTargetUserUHS
              <|> try getTargetUserUH
              <|> try getTargetUserNUH
              <|> try getTargetUserN
    where
      getUnknownTarget = do
        c <- char '*'
        return $ IrcTargetUnknown (singleton c)
      getTargetUserUHS = do
        user <- parseUserUHS
        return $ IrcTargetUser user
      getTargetUserNUH = do
        user <- parseUserNUH
        return $ IrcTargetUser user
      getTargetUserUH = do
        user <- parseUserUH
        return $ IrcTargetUser user
      getTargetUserN = do
        user <- parseUserN
        return $ IrcTargetUser user

parseTargets :: Parser [IrcTarget]
parseTargets = sepBy1 parseTarget separator
    where
      separator = char ','

parseUserUHS :: Parser IrcUser
parseUserUHS = do
  userName <- parseUserName
  char '%'
  hostName <- parseHostName
  char '@'
  server   <- parseHostName
  return $ IrcUser Nothing (Just userName) (Just hostName) (Just server)

parseUserNUH :: Parser IrcUser
parseUserNUH = do
  nickName <- parseNickName
  char '!'
  userName <- parseUserName
  char '@'
  hostName <- parseHostName
  return $ IrcUser (Just nickName) (Just userName) (Just hostName) Nothing

parseUserUH :: Parser IrcUser
parseUserUH = do
  userName <- parseUserName
  char '%'
  hostName <- parseHostName
  return $ IrcUser Nothing (Just userName) (Just hostName) Nothing

parseUserNH :: Parser IrcUser
parseUserNH = do
  nickName <- parseNickName
  char '@'
  hostName <- parseHostName
  return $ IrcUser (Just nickName) Nothing (Just hostName) Nothing

parseUserN :: Parser IrcUser
parseUserN = do
  nickName <- parseNickName
  return $ IrcUser (Just nickName) Nothing Nothing Nothing

parseChannel :: Parser IrcTarget
parseChannel = try getLocalChannel <|> try getGlobalChannel
    where
      getLocalChannel = do
        char '&'
        channel <- getChannelName
        return $ IrcLocalChannel (pack channel)
      getGlobalChannel = do
        char '#'
        channel <- getChannelName
        return $ IrcGlobalChannel (pack channel)
      getChannelName = many (alphaNum <|> oneOf ":-")

parseHostName :: Parser IrcHostName
parseHostName = do
  segments <- sepBy1 parseSegment separator
  return $ IrcHostName $ intercalate "." (map pack segments)
    where
      parseSegment = do
                  segment <- many1 $ alphaNum <|> oneOf "-/"
                  if last segment == '-'
                  then fail "Domain segment can't end with '-'"
                  else return $ segment
      separator = char '.'

parseNickName :: Parser IrcNickName
parseNickName = do
  nickName <- many $ alphaNum <|> oneOf "-."
  return $ IrcNickName (pack nickName)

parseUserName :: Parser IrcUserName
parseUserName = do
  userName <- many $ alphaNum <|> oneOf ".-~+"
  return $ IrcUserName (pack userName)

parseCommand :: Parser IrcCommand
parseCommand = asum commandParser
    where
      commandParser = map makeParser commandsTable
      makeParser (s, c) = try (string s >> return c)

parseParameters :: IrcCommand -> Parser IrcParameters
parseParameters CmdNotice  = parseMsgParameters
parseParameters CmdPrivmsg = parseMsgParameters
parseParameters CmdUser    = parseUserParameters
parseParameters CmdJoin    = try parseChannelAndKeysParameters
                             <|> try parseChannelParameters
parseParameters _          = parseDefaultParameters

parseMsgParameters :: Parser IrcParameters
parseMsgParameters = do
  targets <- parseTargets
  string " :"
  message <- many $ noneOf "\r\n"
  return $ IrcParamMsg targets (pack message)

parseUserParameters :: Parser IrcParameters
parseUserParameters = do
  username <- many alphaNum
  char ' '
  mode <- digit
  realname <- many $ noneOf "\r\n"
  return $ IrcParamUser (pack username) (read [mode] :: Int) (pack realname)

parseChannelAndKeysParameters :: Parser IrcParameters
parseChannelAndKeysParameters = do
  channels <- parseTargets
  char ' '
  keys <- sepBy1 (many $ noneOf ", ") separator
  return $ IrcParamChan channels (Just (map pack keys))
    where
      separator = char ','

parseChannelParameters :: Parser IrcParameters
parseChannelParameters = do
  channels <- parseTargets
  return $ IrcParamChan channels Nothing

parseDefaultParameters :: Parser IrcParameters
parseDefaultParameters = do
  x <- many $ noneOf "\r\n"
  return $ IrcParamDefault $ splitOn " :" (pack x)

parsePrefix :: Parser IrcPrefix
parsePrefix = do
  char ':'
  prefix <- try prefixNUH <|> try prefixNH <|> try prefixH <|> try prefixN
  char ' '
  return $ prefix
    where
      prefixNUH = do
                user <- parseUserNUH
                return $ IrcPrefixUser user
      prefixNH = do
                user <- parseUserNH
                return $ IrcPrefixUser user
      prefixH = do
                hostName <- parseHostName
                return $ IrcPrefixHost hostName
      prefixN = do
                user <- parseUserN
                return $ IrcPrefixUser user

parseMessage :: Parser IrcMessage
parseMessage = do
  prefix <- option IrcPrefixDefault parsePrefix
  command <- parseCommand
  char ' '
  parameters <- optionMaybe $ parseParameters command
  return $ IrcMessage prefix command parameters

