{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), ServerConfig(..), readConfig) where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Yaml as Y
import           Data.Yaml (FromJSON(..), (.:))
import           System.Directory
import           System.FilePath ((</>))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           GHC.Generics (Generic)

data ServerConfig = ServerConfig
    { scAddress  :: String
    , scPort     :: Int
    , scChannels :: [String]
    } deriving (Show, Generic)

data Config = Config
    { cNickname :: String
    , cServer   :: ServerConfig
    } deriving (Show, Generic)

instance FromJSON ServerConfig where
    parseJSON (Y.Object v) =
        ServerConfig <$>
        v .: "address" <*>
        v .: "port"    <*>
        v .: "channels"
    parseJSON _ = fail "Expected Object for ServerConfig value"

instance FromJSON Config where
    parseJSON (Y.Object v) =
        Config <$>
        v .: "nick"   <*>
        v .: "server"
    parseJSON _ = fail "Expected Object for Config value"

readConfig :: IO Config
readConfig = do
  filePath <- getConfigurationFilePath "config.yaml"
  content <- B.readFile $ filePath
  return . fromJust . Y.decode $ content

printConfig :: IO ()
printConfig = do
  config <- readConfig
  print config

getConfigurationFilePath fileName = do
  currentDirectory <- getCurrentDirectory
  exists <- doesFileExist $ currentDirectory </> fileName

  if not exists
     then ioError $ userError $ "Path doesn't exist"
     else return $ currentDirectory </> fileName
