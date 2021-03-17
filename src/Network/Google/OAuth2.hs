{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Google.OAuth2
    ( getAccessToken
    ) where

import Control.Exception.Safe (handleIO, throwString)
import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.OAuth.OAuth2
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import URI.ByteString (serializeURIRef')
import URI.ByteString.QQ (uri)

deriving instance Read IdToken
deriving instance Read AccessToken
deriving instance Read RefreshToken
deriving instance Read OAuth2Token

getAccessToken
    :: Text             -- ^ Client Id
    -> Text             -- ^ Client Secret
    -> [Text]           -- ^ Scopes
    -> Maybe FilePath   -- ^ File in which to cache the token
    -> IO OAuth2Token   -- ^ Refreshed token
getAccessToken clientId clientSecret scopes mPath = do
    mgr <- getGlobalManager
    token <- cached mPath $ do
        code <- prompt $ unlines
            [ ""
            , "Visit the following URL to retrieve a verification code:"
            , ""
            , C8.unpack $ serializeURIRef' $ authorizationUrl oauth2
            , ""
            , "Verification code: "
            ]

        fetchAccessToken' mgr $ ExchangeToken $ T.pack code
    maybe (pure token) (refreshAccessToken' mgr) $ refreshToken token
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = Just clientSecret
        , oauthOAuthorizeEndpoint =
            appendQueryParams
                [ ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob")
                , ("scope", C8.intercalate " " $ map encodeUtf8 scopes)
                ]
                [uri|https://accounts.google.com/o/oauth2/auth|]
        , oauthAccessTokenEndpoint =
            appendQueryParams
                [ ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob")
                ]
                [uri|https://www.googleapis.com/oauth2/v3/token|]
        , oauthCallback = Nothing
        }

    fetchAccessToken' m = fromEither <=< fetchAccessToken m oauth2
    refreshAccessToken' m = fromEither <=< refreshAccessToken m oauth2

    fromEither = either (throwString . show) pure

prompt :: String -> IO String
prompt msg = do
    putStr msg
    hFlush stdout
    getLine

cached :: (Read a, Show a) => Maybe FilePath -> IO a -> IO a
cached Nothing act = act
cached (Just fp) act = do
    mResult <- runMaybeT $ do
        c <- MaybeT $ readFileSafe fp
        MaybeT $ pure $ readMaybe c

    case mResult of
        Just x -> pure x
        _ -> do
            x <- act
            x <$ writeFileSafe fp (show x)

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe = handleIO (const $ pure Nothing) . (Just <$>) . readFile

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe fp = handleIO (const $ pure ()) . writeFile fp
