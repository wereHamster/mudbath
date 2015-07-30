{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core
import           Snap.Http.Server

import           Data.Maybe
import           Data.ByteString.Char8 (pack)
import           Data.UUID
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception
import           Control.Applicative

import           System.Environment
import           System.Directory
import           System.Exit
import           System.Process (CreateProcess, createProcess, proc, waitForProcess)

import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit

import           GitHub
import           GitHub.Types
import           GitHub.WebHook.Handler
import           GitHub.WebHook.Handler.Snap

import           Executor
import           Notifications

import           Prelude


data App = App
    { dispatchScript :: !String
    , githubAccessToken :: !Text
    , httpManager :: !Manager
    , eventQueue :: !(TQueue Event)
    }

mkApp :: IO App
mkApp = App
    <$> locateDispatchScript
    <*> locateGithubAccessToken
    <*> newManager tlsManagerSettings
    <*> newTQueueIO

  where
    locateDispatchScript = do
        getArgs >>= \args -> case args of
            [x] -> return x
            _ -> do
                putStrLn $ "Usage: mudbath <dispatch script>"
                exitFailure

    -- The GitHub access token can be provided either through the environment,
    -- or fetched from instance metadata if running in a cloud.
    --
    -- TODO: Actually implement getting the token from instance metadata.
    locateGithubAccessToken = do
        res <- sequence
            [ lookupEnv "GITHUB_ACCESS_TOKEN"
            ]

        case catMaybes res of
            (token:_) -> return $ T.pack token
            _ -> do
                putStrLn $ "Error: GITHUB_ACCESS_TOKEN must be provided"
                exitFailure


main :: IO ()
main = do
    -- Load configuration and create the App object
    app <- mkApp


    -- Fork the background build thead, Ignore all exceptions, because IO is
    -- dirty and can throw exceptions at any time. And we don't actually care
    -- about those, all we want is to keep the thread running. At any cost.

    void $ forkIO $ forever $
        backgroundBuildThread app `catch` ignoreException


    -- This is the configuration for the webhook handler.
    hookPath <- fromMaybe "webhook" <$> lookupEnv "HOOKPATH"
    mbSecretKey <- lookupEnv "SECRET_TOKEN"

    quickHttpServe $
        webhookHandler (pack hookPath) mbSecretKey
            (handleEvent app) <|> writeText "ok\n"

  where

    backgroundBuildThread app = do
        de <- atomically $ readTQueue $ eventQueue app
        processEvent app de


    ignoreException :: SomeException -> IO ()
    ignoreException e = do
        putStrLn $ "ignoreException: " ++ show e



-- | The handler of the webhook. It parses the request body as an GitHub event
-- and processes it.
--
-- Some events are processed immediately, others are put into the queuer to
-- process them in the background thread.
handleEvent :: App -> Either Error (UUID, Event) -> Snap ()
handleEvent app res = case res of
    Left _ -> do
        writeText "error\n"
    Right (_, ev) -> liftIO $ case ev of
        (DeploymentEventType _) -> atomically $ writeTQueue (eventQueue app) ev
        _                       -> processEvent app ev



processEvent :: App -> Event -> IO ()
processEvent app (DeploymentEventType de) =
    executeDeploymentScript app de

processEvent app (DeploymentStatusEventType dse) =
    notifyDeploymentStatusChange (httpManager app) dse

processEvent _ _ =
    return ()


executeDeploymentScript :: App -> DeploymentEvent -> IO ()
executeDeploymentScript app ev = do
    updateDeploymentStatus (httpManager app) ev Pending

    tmp <- buildDirectory
    deploy app ev tmp


spawn :: CreateProcess -> IO ExitCode
spawn x = do
    (_, _, _, p) <- createProcess x
    waitForProcess p


deploy :: App-> DeploymentEvent -> Text -> IO ()
deploy app de tmp = do
    clone >>= test >>= updateDeploymentStatus (httpManager app) de >> cleanup

  where
    d            = deploymentEventDeployment de
    sha          = deploymentSha d
    repo         = deploymentEventRepository de
    dEnv         = deploymentEnvironment d
    fullRepoName = repositoryFullName repo
    cachePath    = "/var/cache/mudbath/repo/" <> fullRepoName
    url          = "https://" <> githubAccessToken app <> "@github.com/" <> fullRepoName <> ".git"
    script       = "./config/" <> fullRepoName <> "/" <> dEnv

    clone = do
        exitCode <- spawn $ proc "sh" [ "-c", setupScript cachePath tmp url sha ]
        print $ "clone " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Error

    test Error = return Error
    test _ = do
        print $ show $ "executing " <> script
        exitCode <- spawn $ proc (T.unpack script) [T.unpack tmp]
        print $ "test " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Failure

    cleanup = removeDirectoryRecursive $ T.unpack tmp
