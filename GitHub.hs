{-# LANGUAGE OverloadedStrings #-}

module GitHub
    ( updateDeploymentStatus
    ) where


import qualified Control.Exception.Base      as CE

import           Data.Monoid
import qualified Data.Text                   as T

import           System.Environment (getEnv)

import           Network.HTTP.Conduit

import           GitHub.Types
import           Http



getEnvVar :: String -> IO (Either IOError String)
getEnvVar = CE.try . getEnv


-- | Update the deployment status on GitHub. This requires an access token,
-- which is read from the environment.
updateDeploymentStatus :: Manager -> DeploymentEvent -> State -> IO ()
updateDeploymentStatus httpManager de state =
    getEnvVar "GITHUB_ACCESS_TOKEN" >>= either (\_ -> return ()) sendRequest

  where
    dId      = T.pack $ show $ deploymentId $ deploymentEventDeployment de
    repo     = deploymentEventRepository de
    repoName = repositoryFullName repo
    url   = "https://api.github.com/repos/" <> repoName <> "/deployments/" <> dId <> "/statuses"


    sendRequest token = do
        httpPOST httpManager url (T.pack token) $
            CreateDeploymentStatusRequest state Nothing Nothing
