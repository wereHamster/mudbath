{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Notifications
    ( notifyDeploymentStatusChange
    ) where


import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Control.Exception.Base      as CE

import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text                   as T

import           System.Environment (getEnv)

import           Network.HTTP.Conduit (Manager, RequestBody(..), Request(requestBody,requestHeaders,method), httpLbs, parseUrl)
import           Network.HTTP.Types.Method (methodPost)

import           GitHub.Types



-- | Send notification about the deployment status change to all configured
-- targets.
--
-- Configuration is done through environment variables. Currently supported
-- are:
--
--  * slack: SLACK_TEAM, SLACK_TOKEN

notifyDeploymentStatusChange :: Manager -> DeploymentStatusEvent -> IO ()
notifyDeploymentStatusChange httpManager ev = do
    notifySlack httpManager ev



getEnvVar :: String -> IO (Either IOError String)
getEnvVar = CE.try . getEnv



------------------------------------------------------------------------------
-- Slack

data SlackMessage = SlackMessage { smText :: Text }

instance ToJSON SlackMessage where
    toJSON SlackMessage{..} = object [ "text" .= smText ]


notifySlack :: Manager -> DeploymentStatusEvent -> IO ()
notifySlack httpManager ev = do
    mbConfig <- runExceptT $ do
        team  <- ExceptT $ getEnvVar "SLACK_TEAM"
        token <- ExceptT $ getEnvVar "SLACK_TOKEN"
        ExceptT $ return $ Right (team, token)

    case mbConfig of
        Left  _             -> return ()
        Right (team, token) -> sendRequest team token

  where
    ds         = deploymentStatusEventDeploymentStatus ev
    state      = deploymentStatusState           ds
    deployment = deploymentStatusEventDeployment ev
    repo       = deploymentStatusEventRepository ev

    repoOwner  = repositoryOwner repo
    repoName   = repositoryFullName repo

    userName   = ownerLogin repoOwner

    env        = deploymentEnvironment deployment

    sendRequest :: String -> String ->IO ()
    sendRequest team token = do
        req <- parseUrl $ "https://" <> team <> ".slack.com/services/hooks/incoming-webhook?token=" <> token

        let msg = userName <> " is deploying " <> repoName <> " to " <> env
        let text = msg <> ": " <> T.pack (show state)

        let body = RequestBodyLBS $ encode $ SlackMessage text
        let contentType = ("Content-Type","application/json")
        let req' = req { Network.HTTP.Conduit.method = methodPost, requestBody = body, requestHeaders = contentType : requestHeaders req }

        void $ httpLbs req' httpManager
