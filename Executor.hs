{-# LANGUAGE OverloadedStrings #-}

module Executor
    ( executeDeploymentScript
    ) where


import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

import           Control.Applicative
import           Control.Arrow

import           System.Directory
import           System.Exit
import           System.Process (CreateProcess, createProcess, proc, waitForProcess)
import           System.Random

import           GitHub
import           GitHub.Types


executeDeploymentScript :: DeploymentEvent -> IO ()
executeDeploymentScript ev = do
    updateDeploymentStatus ev Pending

    tmp <- buildDirectory
    deploy ev tmp


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
    return $ T.pack $ fst $ randomString 20 stdgen


buildDirectory :: IO Text
buildDirectory = mappend "/tmp/mudbath/build/" <$> generateRandomString



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

spawn :: CreateProcess -> IO ExitCode
spawn x = do
    (_, _, _, p) <- createProcess x
    waitForProcess p


deploy :: DeploymentEvent -> Text -> IO ()
deploy de tmp = do
    clone >>= test >>= updateDeploymentStatus de >> cleanup

  where
    d     = deploymentEventDeployment de
    sha   = deploymentSha d
    repo  = deploymentEventRepository de
    dEnv  = deploymentEnvironment d
    repoName = repositoryFullName repo

    clone = do
        let cachePath = "/tmp/mudbath/cache/" <> repoName
        let url = "git@github.com:" <> repoName <> ".git"
        exitCode <- spawn $ proc "sh" [ "-c", setupScript cachePath tmp url sha ]
        print $ "clone " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Error

    test Error = return Error
    test _ = do
        let script = "./config/" <> repoName <> "/" <> dEnv
        print $ show $ "executing " <> script
        exitCode <- spawn $ proc (T.unpack script) [T.unpack tmp]
        print $ "test " ++ show exitCode
        case exitCode of
            ExitSuccess -> return Success
            _           -> return Failure

    cleanup = removeDirectoryRecursive $ T.unpack tmp
