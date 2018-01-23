module Main (main) where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified System.Environment as Environment
import qualified Text.Printf as Printf

main :: IO ()
main = do
  manager <- Client.newTlsManager
  [slurpUri] <- Environment.getArgs

  packages <- do
    request <- Client.parseUrlThrow "https://hackage.haskell.org/packages/.json"
    response <- Client.httpLbs request manager
    case Aeson.eitherDecode (Client.responseBody response) of
      Left message -> fail message
      Right packages -> pure packages

  Monad.forM_ (map unwrapPackage (List.sort packages)) (\ package -> do
    let
      uri = concat ["PUT ", slurpUri, "/packages/", package]
      location = concat ["https://hackage.haskell.org/package/", package]
      json = Aeson.object
        [ (Text.pack "name", Aeson.toJSON package)
        , (Text.pack "location", Aeson.toJSON location)
        ]
      body = Client.RequestBodyLBS (Aeson.encode json)
    request <- Client.parseRequest uri
    response <- Client.httpNoBody request { Client.requestBody = body } manager
    Printf.printf "%d %s\n"
      (Http.statusCode (Client.responseStatus response))
      package)

newtype Package = Package
  { unwrapPackage :: String
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Package where
  parseJSON = Aeson.withObject "Package" (\ object -> Package
    <$> object Aeson..: Text.pack "packageName")
