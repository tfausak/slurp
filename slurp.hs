module Main (main) where

import qualified Control.Concurrent.STM as Stm
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Distribution.Types.PackageName as Cabal
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  packagesVar <- Stm.newTVarIO (Packages (Map.singleton
    (Maybe.fromJust (toName "base"))
    (Maybe.fromJust (toLocation "https://hackage.haskell.org/package/base"))))
  Warp.run port (application packagesVar)

port :: Warp.Port
port = 8080

application :: Stm.TVar Packages -> Wai.Application
application packagesVar request respond = do
  packages <- Stm.readTVarIO packagesVar
  let
    method = fromUtf8 (Wai.requestMethod request)
    path = map Text.unpack (Wai.pathInfo request)
  case (method, path) of

    ("GET", ["packages"]) ->
      respond (jsonResponse Http.ok200 [] packages)

    ("PUT", ["packages", rawName]) ->
      case toName rawName of
        Nothing -> respond (jsonResponse Http.notFound404 [] Aeson.Null)
        Just name -> if Map.member name (unwrapPackages packages)
          then respond (jsonResponse Http.conflict409 [] Aeson.Null)
          else do
            body <- Wai.lazyRequestBody request
            case Aeson.eitherDecode body of
              Left message -> respond (jsonResponse
                Http.unprocessableEntity422 [] message)
              Right package -> if packageName package /= name
                then respond (jsonResponse Http.badRequest400 [] Aeson.Null)
                else do
                  Stm.atomically (Stm.modifyTVar packagesVar
                    ( Packages
                    . Map.insert (packageName package) (packageLocation package)
                    . unwrapPackages
                    ))
                  respond (jsonResponse Http.created201 [] Aeson.Null)

    ("GET", ["packages", rawName]) ->
      case toName rawName of
        Nothing -> respond (jsonResponse Http.notFound404 [] Aeson.Null)
        Just name -> case Map.lookup name (unwrapPackages packages) of
          Nothing -> respond (jsonResponse Http.notFound404 [] Aeson.Null)
          Just location -> respond (jsonResponse
            Http.found302
            [(Http.hLocation, toUtf8 (fromLocation location))]
            Aeson.Null)

    _ ->
      respond (jsonResponse Http.notFound404 [] Aeson.Null)

jsonResponse
  :: Aeson.ToJSON json
  => Http.Status -> Http.ResponseHeaders -> json -> Wai.Response
jsonResponse status headers body = Wai.responseLBS
  status
  ((Http.hContentType, toUtf8 "application/json") : headers)
  (Aeson.encode body)

newtype Packages = Packages
  { unwrapPackages :: Map.Map Name Location
  } deriving (Eq, Ord, Show)

instance Aeson.ToJSON Packages where
  toJSON packages = Aeson.object
    [ (Text.pack "packages"
      , Aeson.toJSON (map
        (uncurry Package)
        (Map.toAscList (unwrapPackages packages)))
      )
    ]


data Package = Package
  { packageName :: Name
  , packageLocation :: Location
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Package where
  parseJSON = Aeson.withObject "Package" (\ object -> do
    name <- object Aeson..: Text.pack "name"
    location <- object Aeson..: Text.pack "location"
    pure Package
      { packageName = name
      , packageLocation = location
      })

instance Aeson.ToJSON Package where
  toJSON package = Aeson.object
    [ (Text.pack "name", Aeson.toJSON (packageName package))
    , (Text.pack "location", Aeson.toJSON (packageLocation package))
    ]


newtype Name = Name
  { unwrapName :: Cabal.PackageName
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "Name" (\ text -> case toName (Text.unpack text) of
    Nothing -> fail "invalid name"
    Just name -> pure name)

instance Aeson.ToJSON Name where
  toJSON name = Aeson.toJSON (fromName name)

-- TODO: Actually parse package names.
toName :: String -> Maybe Name
toName string = Just (Name (Cabal.mkPackageName string))

fromName :: Name -> String
fromName name = Cabal.unPackageName (unwrapName name)


newtype Location = Location
  { unwrapLocation :: Uri.URI
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Location where
  parseJSON = Aeson.withText "Location" (\ text ->
    case toLocation (Text.unpack text) of
      Nothing -> fail "invalid location"
      Just location -> pure location)

instance Aeson.ToJSON Location where
  toJSON location = Aeson.toJSON (fromLocation location)

-- TODO: Only accept HTTPS URIs.
toLocation :: String -> Maybe Location
toLocation string = case Uri.parseURI string of
  Nothing -> Nothing
  Just uri -> Just (Location uri)

fromLocation :: Location -> String
fromLocation location = Uri.uriToString id (unwrapLocation location) ""


toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)

fromUtf8 :: ByteString.ByteString -> String
fromUtf8 bytes = Text.unpack (Text.decodeUtf8 bytes)