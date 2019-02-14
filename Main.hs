{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Proxy (Proxy(..))
import Data.Text (Text(..))
import Network.Wai.Handler.Warp (run)
import Options.Generic (Generic, ParseRecord, getRecord)
import Servant (Raw, serve, serveDirectoryWebApp)

data Options = Options { from :: FilePath }
    deriving (Generic, ParseRecord)

staticAPI :: Proxy Raw
staticAPI = Proxy

main :: IO ()
main = do
  Options {..} <- getRecord "serve a static directory"
  putStrLn "running on 8080"
  run 8080
    $ serve staticAPI
    $ serveDirectoryWebApp from
