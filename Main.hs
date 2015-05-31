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
import           GitHub.WebHook.Handler
import           GitHub.WebHook.Handler.Snap

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
        webhookHandler (BC8.pack hookPath) mbSecretKey (handleEvent queue) <|> writeText "ok\n"

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
hook :: TQueue Event -> Either Error Event -> Snap ()
hook queue res = case res of
    Left _ -> do
        writeText "error\n"
    Right ev -> liftIO $ case ev of
        (DeploymentEventType _) -> atomically $ writeTQueue queue ev
        _                       -> processEvent ev



processEvent :: Event -> IO ()
processEvent (DeploymentEventType de) =
    executeDeploymentScript de

processEvent (DeploymentStatusEventType dse) =
    notifyDeploymentStatusChange dse

processEvent _ =
    return ()
