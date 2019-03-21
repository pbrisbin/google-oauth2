# Google OAuth2

Interactive Google OAuth2 token negotiation

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson
import Data.Semigroup ((<>))
import Network.Google.OAuth2
import Network.HTTP.Simple
import Network.OAuth.OAuth2
import Data.Text.Excoding (encodeUtf8)

main :: IO ()
main = do
    OAuth2Token{..} <-
      getAccessToken
        "<CLIENT_ID>"      -- Fill with real ID.
        "<CLIENT_SECRET>"  -- Fill with real code.
        ["https://www.googleapis.com/auth/drive"]
        (Just "path/to/credentials.cache")

    request <- parseRequest "https://www.googleapis.com/drive/v2/files"
    response <- httpJSON $ authorize (atoken accessToken) request

    print (getResponseBody response :: Value)
 where
   authorize token = setRequestHeaders
       [ ("Authorization", encodeUtf8 $ "Bearer " <> token)
       ]
```

## Prior Art

This module was inspired by [handa-gdata][] which appears to be abandoned.

[handa-gdata]: http://hackage.haskell.org/package/handa-gdata

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
