{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core
import           Snap.Http.Server

import           Data.Maybe
import qualified Data.ByteString.Char8 as BC8
import           Data.UUID

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception

import           System.Environment

import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit

import           GitHub.Types
import           GitHub.WebHook.Handler
import           GitHub.WebHook.Handler.Snap

import           Executor
import           Notifications

import           Prelude



main :: IO ()
main = do

    -- Create a HTTP manager for the whole app. This is passed around to whoever
    -- needs to send out HTTP requests.
    httpManager <- newManager tlsManagerSettings

    hookPath <- fromMaybe "webhook" <$> lookupEnv "HOOKPATH"
    mbSecretKey <- lookupEnv "SECRET_TOKEN"
    queue <- newTQueueIO


    -- Fork the background build thead, Ignore all exceptions, because IO is
    -- dirty and can throw exceptions at any time. And we don't actually care
    -- about those, all we want is to keep the thread running. At any cost.

    void $ forkIO $ forever $
        backgroundBuildThread httpManager queue `catch` ignoreException

    quickHttpServe $
        webhookHandler (BC8.pack hookPath) mbSecretKey (handleEvent httpManager queue) <|> writeText "ok\n"

  where

    backgroundBuildThread httpManager queue = do
        de <- atomically $ readTQueue queue
        processEvent httpManager de


    ignoreException :: SomeException -> IO ()
    ignoreException e = do
        putStrLn $ "ignoreException: " ++ show e



-- | The handler of the webhook. It parses the request body as an GitHub event
-- and processes it.
--
-- Some events are processed immediately, others are put into the queuer to
-- process them in the background thread.
handleEvent :: Manager -> TQueue Event -> Either Error (UUID, Event) -> Snap ()
handleEvent httpManager queue res = case res of
    Left _ -> do
        writeText "error\n"
    Right (_, ev) -> liftIO $ case ev of
        (DeploymentEventType _) -> atomically $ writeTQueue queue ev
        _                       -> processEvent httpManager ev



processEvent :: Manager -> Event -> IO ()
processEvent httpManager (DeploymentEventType de) =
    executeDeploymentScript httpManager de

processEvent httpManager (DeploymentStatusEventType dse) =
    notifyDeploymentStatusChange httpManager dse

processEvent _ _ =
    return ()
