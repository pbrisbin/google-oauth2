{-# LANGUAGE CPP #-}
module Network.Google.OAuth2
    (
    -- * Types
      Credentials(..)
    , OAuth2Client(..)
    , OAuth2Code
    , OAuth2Scope
    , OAuth2Token
    , OAuth2Tokens(..)

    -- * Getting an access token
    , getAccessToken

    -- * Lower-level steps
    , newCreds
    , refreshCreds
    , promptForCode
    , exchangeCode
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>),(<*>))
#endif

import Control.Arrow (second)
import Control.Monad (mzero, void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
    ( Response(..)
    , httpLbs
    , newManager
    , parseUrlThrow
    , tlsManagerSettings
    , urlEncodedBody
    )
import Network.HTTP.Base (urlEncode)
import System.IO (hFlush, stdout)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

type OAuth2Code = String
type OAuth2Scope = String
type OAuth2Token = String

-- | OAuth2 client definition
--
-- https://developers.google.com/console/help/new/#generatingoauth2
--
data OAuth2Client = OAuth2Client
    { clientId :: !String
    , clientSecret :: !String
    }
    deriving (Read, Show)

data OAuth2Tokens = OAuth2Tokens
    { accessToken :: !OAuth2Token
    , refreshToken :: !OAuth2Token
    , expiresIn :: !Int
    , tokenType :: !String
    }
    deriving (Read, Show)

instance FromJSON OAuth2Tokens where
    parseJSON (Object o) = OAuth2Tokens
        <$> o .: "access_token"
        <*> o .: "refresh_token"
        <*> o .: "expires_in"
        <*> o .: "token_type"

    parseJSON _ = mzero

-- Used only when refreshing a token, where the response lacks the originally
-- supplied refresh_token
data RefreshResponse = RefreshResponse
    { rAccessToken :: OAuth2Token
    , rExpiresIn :: Int
    , rTokenType :: String
    }

instance FromJSON RefreshResponse where
    parseJSON (Object o) = RefreshResponse
        <$> o .: "access_token"
        <*> o .: "expires_in"
        <*> o .: "token_type"

    parseJSON _ = mzero

toOAuth2Tokens :: OAuth2Token -> RefreshResponse -> OAuth2Tokens
toOAuth2Tokens token RefreshResponse{..} =
    OAuth2Tokens
        { accessToken = rAccessToken
        , refreshToken = token
        , expiresIn = rExpiresIn
        , tokenType = rTokenType
        }

-- | Pairs a client and its tokens
--
-- This type is primarily so they can be cached together and not require access
-- the client id when using cached tokens
--
data Credentials = Credentials
    { credsClient :: OAuth2Client
    , credsTokens :: OAuth2Tokens
    }
    deriving (Read, Show)

-- | Get a valid access token with the given scopes
--
-- If given, credentials are cached in a file, thus preventing the need for any
-- prompting on subsequent reuse. N.B. this function always refreshes the access
-- token before returning it.
--
getAccessToken :: OAuth2Client
               -> [OAuth2Scope]
               -> Maybe FilePath -- ^ File in which to cache the token
               -> IO OAuth2Token -- ^ Refreshed token
getAccessToken client scopes (Just tokenFile) = do
    cached <- cachedValue tokenFile
    creds <- case cached of
        Just c -> return c
        Nothing -> newCreds client scopes

    refreshed <- refreshCreds creds
    void $ cacheValue tokenFile refreshed

    return $ accessToken $ credsTokens refreshed

getAccessToken client scopes Nothing = do
    creds <- newCreds client scopes
    refreshed <- refreshCreds creds

    return $ accessToken $ credsTokens refreshed

-- | Prompt the user for a verification code and exchange it for tokens
newCreds :: OAuth2Client -> [OAuth2Scope] -> IO Credentials
newCreds client scopes = do
    code <- promptForCode client scopes
    tokens <- exchangeCode client code

    return $ Credentials client tokens

-- | Prompt the user for a verification code
promptForCode :: OAuth2Client -> [OAuth2Scope] -> IO OAuth2Code
promptForCode client scopes = do
    putStrLn ""
    putStrLn "Visit the following URL to retrieve a verification code:"
    putStrLn ""
    putStrLn $ permissionUrl client scopes
    putStrLn ""
    putStr   "Verification code: "
    hFlush stdout

    getLine

-- | Exchange a verification code for tokens
exchangeCode :: OAuth2Client -> OAuth2Code -> IO OAuth2Tokens
exchangeCode client code = postTokens
    [ ("client_id", clientId client)
    , ("client_secret", clientSecret client)
    , ("grant_type", "authorization_code")
    , ("redirect_uri", redirectUri)
    , ("code", code)
    ]

-- | Use the refresh token to get a new access token
refreshCreds :: Credentials -> IO Credentials
refreshCreds (Credentials client tokens) = do
    refreshed <- postTokens
        [ ("client_id", clientId client)
        , ("client_secret", clientSecret client)
        , ("grant_type", "refresh_token")
        , ("refresh_token", refreshToken tokens)
        ]

    return $ Credentials client
        $ toOAuth2Tokens (refreshToken tokens) refreshed

postTokens :: FromJSON a => [(ByteString, String)] -> IO a
postTokens params = do
    request <- parseUrlThrow "https://accounts.google.com/o/oauth2/token"

    let params' = map (second C8.pack) params

    mngr <- newManager tlsManagerSettings
    unsafeDecode <$> httpLbs (urlEncodedBody params' request) mngr

cachedValue :: Read a => FilePath -> IO (Maybe a)
cachedValue tokenFile = do
    result <- fmap (fmap reads) $ try $ readFile tokenFile

    return $ case result of
        Right ((t,_):_) -> Just t
        _ -> Nothing

cacheValue :: Show a => FilePath -> a -> IO a
cacheValue tokenFile x = fmap (const x) $ try $ writeFile tokenFile (show x)

permissionUrl :: OAuth2Client -> [OAuth2Scope] -> String
permissionUrl client scopes =
    "https://accounts.google.com/o/oauth2/auth"
    <> "?response_type=code"
    <> "&client_id=" <> clientId client
    <> "&redirect_uri=urn:ietf:wg:oauth:2.0:oob"
    <> "&scope=" <> intercalate "+" (map urlEncode scopes)

redirectUri :: String
redirectUri = "urn:ietf:wg:oauth:2.0:oob"

-- With token responses, we assume that if we don't get an HTTP exception, then
-- the response body will parse correctly.
unsafeDecode :: FromJSON a => Response BL.ByteString -> a
unsafeDecode = fromJust . decode . responseBody

try :: IO a -> IO (Either E.IOException a)
try = E.try
