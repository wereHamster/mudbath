{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core
import           Snap.Http.Server

import           Data.Aeson hiding (Success, Error)
import           Data.Aeson.Types (parseMaybe)
import           Data.Text.Encoding

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception

import           GitHub.Types
import           Executor
import           Notifications



main :: IO ()
main = do
    queue <- newTQueueIO

    -- Fork the background build thead, Ignore all exceptions, because IO is
    -- dirty and can throw exceptions at any time. And we don't actually care
    -- about those, all we want is to keep the thread running. At any cost.

    void $ forkIO $ forever $
        backgroundBuildThread queue `catch` ignoreException

    quickHttpServe (requestHandler queue)

  where

    backgroundBuildThread queue = do
        de <- atomically $ readTQueue queue
        processEvent de


    ignoreException :: SomeException -> IO ()
    ignoreException e = do
        putStrLn $ "ignoreException: " ++ show e


    -- Only POST request to /webhook are handled. To everything else we respond
    -- with 200 OK.
    requestHandler queue = post "webhook" (hook queue) <|> writeText "ok\n"
    post p = path p . Snap.Core.method POST


-- | The handler of the webhook. It parses the request body as an GitHub event
-- and processes it.
--
-- Some events are processed immediately, others are put into the queuer to
-- process them in the background thread.
hook :: TQueue Event -> Snap ()
hook queue = do
    hdrs <- headers <$> getRequest
    body <- readRequestBody (100 * 1000)

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
