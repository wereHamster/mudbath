module Main where

import Snap.Core
import Snap.Http.Server

import Data.Aeson hiding (Success, Error)
import Data.ByteString.Lazy (fromChunks)
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C

import Control.Applicative
import Control.Arrow
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Exception
import qualified Control.Exception.Base as CE
import Control.Monad
import Control.Monad.IO.Class

import System.Directory
import System.Environment (getEnv)
import System.Exit
import System.Process (createProcess, proc, waitForProcess, CreateProcess(..))
import qualified System.IO.Error as E
import System.Random

import Network.HTTP.Conduit (RequestBody(..), Request(requestBody,requestHeaders,method), httpLbs, parseUrl, withManager, applyBasicAuth)
import Network.HTTP.Types.Method (methodPost)


data Identity = Identity {
    email, name, username :: String
} deriving (Show)

instance FromJSON Identity where
    parseJSON (Object x) = Identity <$> (x .: "email") <*> (x .: "name") <*> (x .: "username")
    parseJSON _          = mzero


data Commit = Commit {
    id :: String, author, committer :: Identity
} deriving (Show)

instance FromJSON Commit where
    parseJSON (Object x) = Commit <$> (x .: "id") <*> (x .: "author") <*> (x .: "committer")
    parseJSON _          = mzero


data Repository = Repository {
    ownerName, repoName :: String
} deriving (Show)

instance FromJSON Repository where
    parseJSON (Object x) = Repository <$> (x .: "owner" >>= (.: "name")) <*> (x .: "name")
    parseJSON _          = mzero


data Payload = Payload {
    repository :: Repository, ref, before, after :: String, commits :: [ Commit ]
} deriving (Show)

instance FromJSON Payload where
    parseJSON (Object x) = Payload <$> (x .: "repository") <*> (x .: "ref") <*> (x .: "before") <*> (x .: "after") <*> (x .: "commits")
    parseJSON _          = mzero


data Status = Pending | Success | Failure | Error deriving (Eq, Show)

showBS Pending = "pending"
showBS Success = "success"
showBS Failure = "failure"
showBS Error   = "error"

getEnvVar :: String -> IO (Either IOError String)
getEnvVar = CE.try . getEnv


setupScript cachePath buildPath url commit = Data.List.intercalate ";" [
        "set -e",
        "if test -d " ++ cachePath,
        "then cd " ++ cachePath ++ " && git fetch --all --quiet",
        "else git clone --mirror --quiet " ++ url ++ " " ++ cachePath,
        "fi",
        "rm -rf " ++ buildPath,
        "git clone --quiet --reference " ++ cachePath ++ " " ++ url ++ " " ++ buildPath,
        "cd " ++ buildPath,
        "git checkout -q " ++ commit
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

generateRandomString :: IO String
generateRandomString = do
        stdgen <- newStdGen
        return $ fst $ randomString 10 stdgen

build :: Payload -> Commit -> IO ()
build payload commit = do
    notify Pending

    -- Generate a random string, it's used to create a directory in which
    -- this build takes place. When done, we clean up this directory.
    buildId <- generateRandomString
    let path = "/tmp/mudbath/build/" ++ buildId

    clone path >>= test path >>= notify >> cleanup path

  where
    repo  = repository payload
    id    = Main.id commit
    owner = ownerName repo
    name  = repoName repo

    spawn x = do
        (_, _, _, p) <- createProcess x
        waitForProcess p

    clone path = do
        let cachePath = "/tmp/mudbath/cache/" ++ name
        let url = "git@github.com:" ++ owner ++ "/" ++ name ++ ".git"
        exitCode <- spawn $ proc "sh" [ "-c", setupScript cachePath path url id ]
        case exitCode of
            ExitSuccess -> return Success
            otherwise   -> return Error

    test _ Error = return Error
    test path _ = do
        exitCode <- spawn $ (proc "sh" [ "-c", "set -e; npm install 1>/dev/null 2>&1; make test" ]) { cwd = Just path }
        case exitCode of
            ExitSuccess -> return Success
            otherwise   -> return Failure

    notify status = do
        updateCommitStatus repo id status
        when (status /= Pending) $ notifyCampfire payload commit status

    cleanup path = do
        removeDirectoryRecursive path

updateCommitStatus :: Repository -> String -> Status -> IO ()
updateCommitStatus repo id status = do
    getEnvVar "GITHUB" >>= either (\x -> return ()) sendRequest

  where

    sendRequest token = do
        let owner = ownerName repo
        let name  = repoName repo

        let url = "https://api.github.com/repos/" ++ owner ++ "/" ++ name ++ "/statuses/" ++ id
        req <- parseUrl url

        let body = RequestBodyLBS $ fromChunks [ "{\"state\":\"", (showBS status), "\"}" ]
        let contentType = ("Content-Type","application/json")
        let tokenHeader = ("Authorization", C.pack $ "token " ++ token)
        let req' = req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = contentType : tokenHeader : requestHeaders req }

        -- resp <- withManager $ httpLbs req'
        return ()

notifyCampfire payload commit status = do
    getEnvVar "CAMPFIRE" >>= either (\x -> return ()) sendRequest

  where

    sendRequest config = do
        let [ domain, token, room ] = Data.List.words config
        req <- parseUrl $ "https://" ++ domain ++ ".campfirenow.com/room/" ++ room ++ "/speak.json"

        let msg = "Build " ++ (ref payload) ++ "@" ++ (Data.List.take 7 . Main.id $ commit)
        let message = msg ++ ": " ++ (show $ status)
        putStrLn message

        let body = RequestBodyBS $ C.pack $ "{\"message\":\"" ++ message ++ "\"}"
        let contentType = ("Content-Type","application/json")
        let req' = applyBasicAuth (C.pack token) "X" $ req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = contentType : requestHeaders req }

        resp <- withManager $ httpLbs req'
        return ()



hook :: TQueue Payload -> Snap ()
hook queue = do
    payload <- getParam "payload"
    case payload of
        Nothing -> pass
        Just x  -> do
            case (decode $ fromChunks [ x ]) of
                Nothing -> pass
                Just x  -> liftIO $ queueBuild x >> return ()

  where

    queueBuild payload = atomically $ writeTQueue queue payload



main :: IO ()
main = do
    queue <- newTQueueIO
    forkIO $ backgroundBuildThread queue
    quickHttpServe (app queue)

  where

    -- The background build thread reads payloads from the queue and builds
    -- each one. It builds each commit individually, not just the last one.
    backgroundBuildThread queue = do
        payload <- atomically $ readTQueue queue
        mapM (build payload) (commits payload)
        backgroundBuildThread queue

    -- Only POST request to /hook are handled. To everything else we respond
    -- with 200 OK.
    app queue = post "hook" (hook queue) <|> writeText "ok\n"
    post dir = (path dir) . (Snap.Core.method POST)
