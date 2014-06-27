{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core
import           Snap.Http.Server

import           Data.Aeson hiding (Success, Error)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Monoid

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import qualified Control.Exception.Base as CE
import           Control.Monad
import           Control.Monad.IO.Class

import           System.Directory
import           System.Environment (getEnv)
import           System.Exit
import           System.Process (CreateProcess, createProcess, proc, waitForProcess)
import           System.Random

import           Network.HTTP.Conduit (RequestBody(..), Request(requestBody,requestHeaders,method), httpLbs, parseUrl, withManager, applyBasicAuth)
import           Network.HTTP.Types.Method (methodPost)

import           GitHub.Types


getEnvVar :: String -> IO (Either IOError String)
getEnvVar = CE.try . getEnv


setupScript :: Text -> Text -> Text -> Text -> String
setupScript cachePath buildPath url commit = T.unpack $ mconcat $ Data.List.intersperse ";"
    [ "set -e"
    , "if test -d " <> cachePath
    , "then cd " <> cachePath <> " && git fetch --all --quiet"
    , "else git clone --mirror --quiet " <> url <> " " <> cachePath
    , "fi"
    , "rm -rf " <> buildPath
    , "git clone --quiet --reference " <> cachePath <> " " <> url <> " " <> buildPath
    , "cd " <> buildPath
    , "git checkout -q " <> commit
    ]

randomString :: RandomGen d => Int -> d -> (String, d)
randomString len =
    first (Data.List.map toChar) . sequence' (Data.List.replicate len (randomR (0, 61)))
  where
    sequence' [] g = ([], g)
    sequence' (f:fs) g =
        let (f', g') = f g
            (fs', g'') = sequence' fs g'
         in (f' : fs', g'')
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

generateRandomString :: IO Text
generateRandomString = do
    stdgen <- newStdGen
    return $ T.pack $ fst $ randomString 10 stdgen


buildDirectory :: IO Text
buildDirectory = mappend "/tmp/mudbath/build/" <$> generateRandomString


spawn :: CreateProcess -> IO ExitCode
spawn x = do
    (_, _, _, p) <- createProcess x
    waitForProcess p


processEvent :: Event -> IO ()
processEvent (DeploymentEventType de) = do
    updateDeploymentStatus de Pending

    tmp <- buildDirectory
    deploy de tmp

processEvent (DeploymentStatusEventType dse) = do
    notify dse

processEvent _ = do
    return ()


notify :: DeploymentStatusEvent -> IO ()
notify dse = do
    token <- getEnvVar "SLACK_TOKEN"
    team  <- getEnvVar "SLACK_TEAM"

    case (team, token) of
        (Right t, Right tok) -> sendRequest t tok
        _ -> return ()

  where
    state      = deploymentStatusEventState      dse
    deployment = deploymentStatusEventDeployment dse
    --repo       = deploymentStatusEventRepository dse

    repoOwner  = "???" -- repositoryOwnerName repo
    repoName   = "???" -- repositoryName      repo

    env        = deploymentEnvironment deployment

    sendRequest team token = do
        req <- parseUrl $ "https://" <> team <> ".slack.com/services/hooks/incoming-webhook?token=" <> token

        let msg = "Somebody is deploying " <> repoOwner <> "/" <> repoName <> " to " <> env
        let text = msg <> ": " <> T.pack (show state)

        let body = RequestBodyBS $ C.pack $ T.unpack $ "{\"text\":\"" <> text <> "\"}"
        let contentType = ("Content-Type","application/json")
        let req' = req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = contentType : requestHeaders req }

        void $ withManager $ httpLbs req'



deploy :: DeploymentEvent -> Text -> IO ()
deploy de tmp = do
    clone >>= test >>= updateDeploymentStatus de >> cleanup

  where
    sha   = deploymentEventSha de
    repo  = deploymentEventRepository de
    dEnv  = deploymentEventEnvironment de
    owner = repositoryOwnerName repo
    name  = repositoryName repo

    clone = do
        let cachePath = "/tmp/mudbath/cache/" <> name
        let url = "git@github.com:" <> owner <> "/" <> name <> ".git"
        exitCode <- spawn $ proc "sh" [ "-c", setupScript cachePath tmp url sha ]
        print $ "clone " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Error

    test Error = return Error
    test _ = do
        let script = "./config/" <> owner <> "/" <> name <> "/" <> dEnv
        exitCode <- spawn $ proc (T.unpack script) [T.unpack tmp]
        print $ "test " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Failure

    cleanup = removeDirectoryRecursive $ T.unpack tmp


updateDeploymentStatus :: DeploymentEvent -> State -> IO ()
updateDeploymentStatus de state =
    getEnvVar "GITHUB" >>= either (\_ -> return ()) sendRequest

  where
    dId   = T.pack $ show $ deploymentEventId de
    repo  = deploymentEventRepository de
    owner = repositoryOwnerName repo
    name  = repositoryName repo
    url   = "https://api.github.com/repos/" <> owner <> "/" <> name <> "/deployments/" <> dId <> "/statuses"


    sendRequest token = do
        req <- parseUrl (T.unpack url)

        let body = RequestBodyLBS $ encode $ CreateDeploymentStatusRequest state Nothing Nothing
        let userAgent   = ("User-Agent", BS.concat [ "mudbath/0" ])
        let contentType = ("Content-Type","application/json")
        let acceptHeader = ("Accept", "application/vnd.github.cannonball-preview+json")
        let req' = applyBasicAuth (C.pack token) "x-oauth-basic" $ req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = userAgent : contentType : acceptHeader : requestHeaders req }

        void $ withManager $ httpLbs req'



hook :: TQueue Event -> Snap ()
hook queue = do
    hdrs <- headers <$> getRequest
    body <- readRequestBody (100 * 1000)

    let mbEvent = do
        eventName <- getHeader "X-GitHub-Event" hdrs
        value     <- decode body
        parseMaybe (eventParser $ decodeUtf8 eventName) value

    case mbEvent :: Maybe Event of
        Nothing -> return ()
        Just ev -> case ev of
            (DeploymentEventType _) -> void $ liftIO $ queueBuild ev
            _                       -> void $ liftIO $ processEvent ev

  where

    queueBuild de = atomically $ writeTQueue queue de



main :: IO ()
main = do
    queue <- newTQueueIO
    void $ forkIO $ backgroundBuildThread queue
    quickHttpServe (requestHandler queue)

  where

    -- The background build thread reads payloads from the queue and builds
    -- each one. It builds each commit individually, not just the last one.
    backgroundBuildThread queue = do
        de <- atomically $ readTQueue queue
        processEvent de
        backgroundBuildThread queue

    -- Only POST request to /webhook are handled. To everything else we respond
    -- with 200 OK.
    requestHandler queue = post "webhook" (hook queue) <|> writeText "ok\n"
    post p = path p . Snap.Core.method POST
