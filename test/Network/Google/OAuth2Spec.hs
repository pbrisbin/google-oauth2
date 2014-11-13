module Network.Google.OAuth2Spec (main, spec) where

import Test.Hspec
import Network.Google.OAuth2

import Control.Applicative
import Data.Monoid
import LoadEnv
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)
import System.Environment

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Token exchange" $ do
        it "prompts the user and returns an access token" $ do
            pendingWith "Requires interaction. Do not run regularly"

            loadEnv

            client <- OAuth2Client
                <$> getEnv "CLIENT_ID"
                <*> getEnv "CLIENT_SECRET"

            scope <- getEnv "EXAMPLE_SCOPE"
            token <- getAccessToken client [scope] Nothing

            token `shouldSatisfy` (not . null)

        -- N.B. requires interaction on first run only
        it "grants access to an API" $ do
            loadEnv

            client <- OAuth2Client
                <$> getEnv "CLIENT_ID"
                <*> getEnv "CLIENT_SECRET"

            scope <- getEnv "EXAMPLE_SCOPE"
            token <- getAccessToken client [scope] (Just "test/oauth.token")

            request <- parseUrl =<< getEnv "EXAMPLE_URL"
            response <- withManager $ httpLbs $ authorize token request

            responseBody response `shouldSatisfy` (not . L8.null)

authorize :: OAuth2Token -> Request -> Request
authorize token request = request
    { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }
