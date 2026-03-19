#!/bin/bash

mkdir -p parser-demo
cd parser-demo
cabal init --non-interactive \
  --package-name=parser-demo \
  --license=MIT \
  --libandexe \
  --tests \
  --main-is=LoxCli.hs \
  --language=GHC2021
