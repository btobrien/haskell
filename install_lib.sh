#!/bin/bash

tar xvf "$1".tar.gz
cd "$1"
runhaskell Setup configure
runhaskell Setup build
sudo runhaskell Setup install
