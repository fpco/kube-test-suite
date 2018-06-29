# The Kubernetes Test Suite

## General 

A set of Haskell stack scripts for testing and validating Kubernetes deployments.

## Dependencies

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)

## Instructions for running

Currently you can simply run the Haskell files as executables.

For example, to run the Kubernetes tests in `KubeTests.hs`
run the following command from your shell:

```
./KubeTests.hs
```

The first time this is run it will install the library dependencies 
using [Stack](https://docs.haskellstack.org/) and then run the tests.

When the test is later subsequently run in the future it will **not*** need to 
download dependencies (and will be much faster).
