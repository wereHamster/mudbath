{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core
import           Snap.Http.Server

import           Crypto.Hash

import           Data.Monoid
import           Data.Aeson hiding (Success, Error)
import           Data.Aeson.Types (parseMaybe)
import           Data.Text.Encoding
import           Data.Maybe
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as BC8

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception

import           System.Environment

import           GitHub.Types
import           Executor
import           Notifications



main :: IO ()
main = do

    hookPath <- fromMaybe "webhook" <$> lookupEnv "HOOKPATH"
    mbSecretKey <- lookupEnv "SECRET_TOKEN"
    queue <- newTQueueIO

    -- Fork the background build thead, Ignore all exceptions, because IO is
    -- dirty and can throw exceptions at any time. And we don't actually care
    -- about those, all we want is to keep the thread running. At any cost.

    void $ forkIO $ forever $
        backgroundBuildThread queue `catch` ignoreException

    quickHttpServe $
        path (BC8.pack hookPath) (method POST $ hook queue mbSecretKey) <|> writeText "ok\n"

  where

    backgroundBuildThread queue = do
        de <- atomically $ readTQueue queue
        processEvent de


    ignoreException :: SomeException -> IO ()
    ignoreException e = do
        putStrLn $ "ignoreException: " ++ show e



-- | The handler of the webhook. It parses the request body as an GitHub event
-- and processes it.
--
-- Some events are processed immediately, others are put into the queuer to
-- process them in the background thread.
hook :: TQueue Event -> Maybe String -> Snap ()
hook queue mbSecretKey = do
    hdrs <- headers <$> getRequest

    body <- do
        body <- readRequestBody (100 * 1000)
        case (mbSecretKey, getHeader "X-Hub-Signature" hdrs) of

            -- No secret key and no signature. Pass along the body unverified.
            (Nothing, Nothing) -> return body

            -- Signature is available but no secret key to verify it. Log this
            -- and accept the body.
            (Nothing, Just _) -> do
                liftIO $ putStrLn $
                    "Ignoring signature because the secret token was not provided"
                return body

            -- Secret token is available but the request is not signed. Reject
            -- the request.
            (Just _, Nothing) -> do
                writeText "Signature missing"
                res <- getResponse
                finishWith $ setResponseCode 400 res

            -- Both the signature and secret token are available. Verify the
            -- signature and reject the request if that fails.
            (Just sc, Just sig) -> do
                let mac = hmac (BC8.pack sc) (LBS.toStrict body) :: HMAC SHA1
                if sig == ("sha1=" <> digestToHexByteString (hmacGetDigest mac))
                    then return body
                    else do
                        liftIO $ do
                            putStrLn "Signature does not match:"
                            print $ show sig
                            print $ digestToHexByteString $ hmacGetDigest mac

                        writeText "Signature does not match"
                        res <- getResponse
                        finishWith $ setResponseCode 400 res


    let mbEvent = do
        eventName <- getHeader "X-GitHub-Event" hdrs
        value     <- decode body
        parseMaybe (eventParser $ decodeUtf8 eventName) value

    case mbEvent of
        Nothing -> return ()
        Just ev -> void $ liftIO $ case ev of
            (DeploymentEventType _) -> atomically $ writeTQueue queue ev
            _                       -> processEvent ev



processEvent :: Event -> IO ()
processEvent (DeploymentEventType de) =
    executeDeploymentScript de

processEvent (DeploymentStatusEventType dse) =
    notifyDeploymentStatusChange dse

processEvent _ =
    return ()
