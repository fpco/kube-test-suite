# The Kubernetes Test Suite

Copyright (c) 2018 FP Complete. All rights reserved.

## General 

WIP

Currently a basic Haskell binary for testing and validating Kubernetes deployments.

## Dependencies

- [stack](https://docs.haskellstack.org/en/stable/README/)
- [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)

## Instructions for running

```
cd faros
stack build && stack exec kube-tests
```

## Note

Because Kubernetes (κυβερνήτης) is Greek for "helmsman" or "pilot", it seemed
appropriate to name a Kubernetes testing library fáros ([φάρος](https://en.wiktionary.org/wiki/%CF%86%CE%AC%CF%81%CE%BF%CF%82)) which is
Greek for "lighthouse".
