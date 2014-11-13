module Network.Google.OAuth2
    (
    -- * Types
      OAuth2Client(..)
    , OAuth2Code
    , OAuth2Scope
    , OAuth2Token
    , OAuth2Tokens(..)

    -- * Getting an access token
    , getAccessToken

    -- * Lower-level steps
    , newTokens
    , refreshTokens
    , promptForCode
    , exchangeCode
    ) where

import Control.Arrow (second)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
    ( Response(..)
    , httpLbs
    , parseUrl
    , withManager
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
    cached <- cachedTokens tokenFile
    tokens <- case cached of
        Just t -> return t
        Nothing -> newTokens client scopes

    refreshed <- refreshTokens client tokens
    void $ cacheTokens tokenFile refreshed

    return $ accessToken refreshed

getAccessToken client scopes Nothing = do
    tokens <- newTokens client scopes
    refreshed <- refreshTokens client tokens

    return $ accessToken refreshed

-- | Prompt the user for a verification code and exchange it for tokens
newTokens :: OAuth2Client -> [OAuth2Scope] -> IO OAuth2Tokens
newTokens client scopes = do
    code <- promptForCode client scopes
    tokens <- exchangeCode client code

    return tokens

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
refreshTokens :: OAuth2Client -> OAuth2Tokens -> IO OAuth2Tokens
refreshTokens client tokens = do
    refreshed <- postTokens
        [ ("client_id", clientId client)
        , ("client_secret", clientSecret client)
        , ("grant_type", "refresh_token")
        , ("refresh_token", refreshToken tokens)
        ]

    return $ toOAuth2Tokens (refreshToken tokens) refreshed

postTokens :: FromJSON a => [(ByteString, String)] -> IO a
postTokens params = do
    request <- parseUrl "https://accounts.google.com/o/oauth2/token"

    let params' = map (second C8.pack) params

    fmap unsafeDecode $ withManager $ httpLbs $ urlEncodedBody params' request

cachedTokens :: FilePath -> IO (Maybe OAuth2Tokens)
cachedTokens tokenFile = do
    result <- fmap (fmap reads) $ try $ readFile tokenFile

    return $ case result of
        Right ((t,_):_) -> Just t
        _ -> Nothing

cacheTokens :: FilePath -> OAuth2Tokens -> IO OAuth2Tokens
cacheTokens tokenFile t = fmap (const t) $ try $ writeFile tokenFile (show t)

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
