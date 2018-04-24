{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Google.OAuth2Spec
    ( main
    , spec
    ) where

import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import LoadEnv
import Network.Google.OAuth2
import Network.HTTP.Simple
import Network.OAuth.OAuth2 (AccessToken(..), OAuth2Token(..))
import System.Environment

-- | To run in an interactive way
--
-- So you can perform the OAuth2 flow
--
-- > stack exec runghc test/Network/Google/OAuth2Spec.hs
--
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Token exchange" $ do
    it "grants access to an API" $ do
        loadEnv
        clientId <- T.pack <$> getEnv "CLIENT_ID"
        clientSecret <- T.pack <$> getEnv "CLIENT_SECRET"
        scope <- T.pack <$> getEnv "EXAMPLE_SCOPE"
        exampleUrl <- getEnv "EXAMPLE_URL"

        OAuth2Token{..} <- getAccessToken clientId clientSecret [scope]
            $ Just "test/oauth.token"

        request <- parseRequest exampleUrl
        response <- httpLBS $ authorize (atoken accessToken) request
        getResponseBody response `shouldSatisfy` (not . L8.null)

authorize :: Text -> Request -> Request
authorize token = setRequestHeaders
    [ ("Authorization", encodeUtf8 $ "Bearer " <> token)
    ]
