# The Kubernetes Test Suite

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
