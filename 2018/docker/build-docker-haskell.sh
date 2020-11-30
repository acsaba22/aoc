#!/bin/bash

set -eu


IMAGE="devhaskell"

export USER_UID=$(id -u)
export USER_GID=$(id -g)
export WORKDIR="$HOME"

docker build --build-arg USER_UID --build-arg USER_GID --build-arg WORKDIR --tag=$IMAGE .
