{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
module Lib
    ( kubeTest
    ) where

import RIO
import RIO.Text (encodeUtf8)
import RIO.ByteString.Lazy (fromStrict)
import Turtle (shellStrict, empty, need, fromText)
import Data.Aeson
import GHC.Generics (Generic)
import Test.Hspec

data Nodes = Nodes
  { apiVersion :: Text
  , itemsNodes :: [Node]
  } deriving (Show, Eq, Generic)

data Node = Node
  { nodeName :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Nodes where
  parseJSON = withObject "Nodes" $ \o -> do
    apiVersion <- o .: "apiVersion"
    itemsNodes <- o .: "items"
    return Nodes{..}

instance FromJSON Node where
  parseJSON = withObject "Node" $ \o -> do
    metadata <- o .: "metadata"
    nodeName     <- metadata .: "name"
    return Node{..}

data Services = Services
  { itemsServices :: [Service]
  } deriving (Show, Eq, Generic)

data Service = Service
  { serviceName :: Text
  , creationTimestamp :: Text
  , clusterIP :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Services where
  parseJSON = withObject "Services" $ \o -> do
    itemsServices <- o .: "items"
    return Services{..}

instance FromJSON Service where
  parseJSON = withObject "Service" $ \o -> do
    metadata <- o .: "metadata"
    serviceName <- metadata .: "name"
    creationTimestamp <- metadata .: "creationTimestamp"
    spec <- o .: "spec"
    clusterIP <- spec .: "clusterIP"
    return Service{..}

getNodes :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack) => m ()
getNodes = do
  (_exitCode, kubeGetNodesText) <- shellStrict "kubectl -o=json get nodes" empty
  let kubeGetNodesByteString = fromStrict $ encodeUtf8 $ kubeGetNodesText
      parsedNodes :: Either String Nodes
      parsedNodes = eitherDecode kubeGetNodesByteString
  case parsedNodes of
    Left e -> logError $ "There was an error attempting to parse" <> fromString e
    Right nodes -> do
      logInfo "Success"
      logInfo $
        "Found the following nodes on a cluster running api version: " <>
          (displayShow $ apiVersion nodes)
      mapM_ (logInfo . displayShow . nodeName) (itemsNodes nodes)
  liftIO $ hspec $ do
    describe "Test to list the names of nodes in the kubernetes cluster" $ do
      it "The list of names of the nodes in the cluster is not empty" $ do
        case parsedNodes of
          Right nodes -> do
            (map nodeName (itemsNodes nodes))
              `shouldSatisfy`
              (\i -> (not $ null i))

getServices :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack) => m ()
getServices = do
  (_exitCode, kubeGetServicesText) <- shellStrict "kubectl -o=json get services" empty
  let kubeGetServicesByteString = fromStrict $ encodeUtf8 $ kubeGetServicesText
      parsedServices :: Either String Services
      parsedServices = eitherDecode kubeGetServicesByteString
  case parsedServices of
    Left e ->
      logError $ "Error while parsing JSON returned from `get services`.\
                 \ There was an error attempting to parse" <> fromString e
    Right services -> do
      logInfo "Success. Found services on the target cluster"
      logInfo $ "\nFound the following service names:"
      mapM_ (logInfo . displayShow . serviceName) (itemsServices services)
      logInfo $ "\nFound the following service creation timestamps:"
      mapM_ (logInfo . displayShow . creationTimestamp) (itemsServices services)
      logInfo $ "\nFound the following cluster IP addresses for the services:"
      mapM_ (logInfo . displayShow . clusterIP) (itemsServices services)
      -- Eventually we will reorganize the above reporting and following Hspec tests
      liftIO $ hspec $ do
        describe "Test the services in the Kubernetes cluster" $ do
          it "The list of names of services in the cluster is not empty" $ do
            (map serviceName (itemsServices services))
              `shouldSatisfy`
              (not . null)
          it "The list of cluster IP addresses for the services is not empty" $ do
            (map clusterIP (itemsServices services))
              `shouldSatisfy`
              (not . null)

kubeTest :: IO ()
kubeTest = do
  clusterName <- need "CLUSTER_NAME"
  kubeConfig  <- need "KUBECONFIG"

  lo <- logOptionsHandle stdout False
  withLogFunc lo $ \logFn -> do
    runRIO logFn $ do
      case (clusterName, kubeConfig) of
        (Nothing, Nothing) ->
          logError "Missing CLUSTER_NAME and KUBECONFIG environmental variables."
        (Just _, Nothing) ->
          logError "Missing KUBECONFIG environmental variable."
        (Nothing, Just _) ->
          logError "Missing KUBECONFIG environmental variable."
        (Just _, Just _) -> do
          logInfo "Running tests on nodes.\n"
          getNodes
          logInfo "\nNow running tests on services.\n"
          getServices
