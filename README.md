# The Kubernetes Test Suite

Copyright (c) 2018 FP Complete. All rights reserved.

## General 

A set of Haskell stack scripts for testing and validating Kubernetes deployments.

## Dependencies

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)

## Instructions for running

```
cd faros
stack build && stack exec kube-tests
```
