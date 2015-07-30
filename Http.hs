{-# LANGUAGE OverloadedStrings #-}

module Http
    ( httpPOST
    ) where


import           Control.Monad

import           Data.Aeson
import           Data.Text
import qualified Data.Text                   as T
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString             as BS

import           Network.HTTP.Conduit (Manager, RequestBody(..), Request(requestBody,requestHeaders,method), httpLbs, parseUrl, applyBasicAuth)
import           Network.HTTP.Types.Method (methodPost)



httpPOST :: (ToJSON a) => Manager -> Text -> Text -> a -> IO ()
httpPOST httpManager url token obj = do
    req0 <- parseUrl (T.unpack url)
    void $ httpLbs (req1 req0) httpManager
  where
    body         = RequestBodyLBS $ encode obj
    userAgent    = ("User-Agent", BS.concat [ "mudbath/0" ])
    contentType  = ("Content-Type","application/json")
    acceptHeader = ("Accept", "application/vnd.github.cannonball-preview+json")
    req1         = \req -> applyBasicAuth (C.pack $ T.unpack token) "x-oauth-basic" $ req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = userAgent : contentType : acceptHeader : requestHeaders req }
