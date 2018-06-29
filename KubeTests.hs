#!/usr/bin/env stack
{- stack
   script
   --resolver lts-11.15
   --package turtle
   --package aeson
   --package hspec
   --package text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- Swap `script` with `exec ghci` to load script into ghci REPL
-- If on NixOS add --nix and --no-nix-pure

import Turtle
import Data.Aeson
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
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

getNodes :: IO ()
getNodes = do
  (_exitCode, kubeGetNodesText) <- shellStrict "kubectl -o=json get nodes" empty
  let kubeGetNodesByteString = encodeUtf8 $ fromStrict $ kubeGetNodesText
      parsedNodes :: Either String Nodes
      parsedNodes = eitherDecode kubeGetNodesByteString
  case parsedNodes of
    Left e -> putStrLn $ "There was an error attempting to parse" <> e
    Right nodes -> do
      putStrLn "Success"
      putStrLn $
        "Found the following nodes on a cluster running api version: " <>
          (show $ apiVersion nodes)
      mapM_ (putStrLn . show . nodeName) (itemsNodes nodes)
  hspec $ do
    describe "Test to list the names of nodes in the kubernetes cluster" $ do
      it "The list of names of the nodes in the cluster is not empty" $ do
        case parsedNodes of
          Right nodes -> do
            (map nodeName (itemsNodes nodes))
              `shouldSatisfy`
              (\i -> (not $ null i))

getServices :: IO ()
getServices = do
  (_exitCode, kubeGetServicesText) <- shellStrict "kubectl -o=json get services" empty
  let kubeGetServicesByteString = encodeUtf8 $ fromStrict $ kubeGetServicesText
      parsedServices :: Either String Services
      parsedServices = eitherDecode kubeGetServicesByteString
  case parsedServices of
    Left e ->
      putStrLn $ "Error while parsing JSON returned from `get services`.\
                 \ There was an error attempting to parse" <> e
    Right services -> do
      putStrLn "Success. Found services on the target cluster"
      putStrLn $ "\nFound the following service names:"
      mapM_ (putStrLn . show . serviceName) (itemsServices services)
      putStrLn $ "\nFound the following service creation timestamps:"
      mapM_ (putStrLn . show . creationTimestamp) (itemsServices services)
      putStrLn $ "\nFound the following cluster IP addresses for the services:"
      mapM_ (putStrLn . show . clusterIP) (itemsServices services)
      -- Eventually we will reorganize the tests and following Hspec reporting
      hspec $ do
        describe "Test the services in the Kubernetes cluster" $ do
          it "The list of names of services in the cluster is not empty" $ do
            (map serviceName (itemsServices services))
              `shouldSatisfy`
              (not . null)
          it "The list of cluster IP addresses for the services is not empty" $ do
            (map clusterIP (itemsServices services))
              `shouldSatisfy`
              (not . null)

-- TODO
-- getPods :: IO ()
-- getPods = undefined

main = do
  clusterName <- need "CLUSTER_NAME"
  kubeConfig  <- need "KUBECONFIG"

  -- TODO
  -- Eventually allow KUBECONFIG and/or CLUSTER_NAME to be set interactively
  -- if they are not set in the env.

  case (clusterName, kubeConfig) of
    (Nothing, Nothing) ->
      putStrLn "Missing CLUSTER_NAME and KUBECONFIG environmental variables."
    (Just _, Nothing) ->
      putStrLn "Missing KUBECONFIG environmental variable."
    (Nothing, Just _) ->
      putStrLn "Missing KUBECONFIG environmental variable."
    (Just _, Just _) -> do
      putStrLn "Running tests on nodes.\n"
      getNodes
      putStrLn "\nNow running tests on services.\n"
      getServices
