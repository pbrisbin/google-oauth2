# Google OAuth2

Google OAuth2 token negotiation

## Installation

```
% cabal install google-oauth2
```

## Usage

```haskell
import Data.Monoid
import Network.Google.OAuth2
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let client = OAuth2Client clientId clientSecret
        scopes = ["https://www.googleapis.com/auth/drive"]

    token <- getAccessToken client scopes Nothing

    request <- parseUrl "https://www.googleapis.com/drive/v2/files"
    response <- withManager $ httpLbs $ authorize token request

    L8.putStrLn $ responseBody response

 where
   authorize token request = request
       { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }

   -- Setup in Google Developers Console
   clientId = "..."
   clientSecret = "..."
```

## Developing and Tests

```
% cp .env{.example,} # and edit it accordingly
% cabal install --dependencies-only --enable-tests -j
% cabal test
```

Note: The tests require some degree of user interaction. One test will always
need it (that's the aspect it's testing) and is therefore pending by default.
The other test will need interaction the first time its run only.

## Prior Art

This module was inspired by [handa-gdata][] which appears to be abandoned.

[handa-gdata]: http://hackage.haskell.org/package/handa-gdata
