#!/bin/bash

set -eu


XAUTHORITY="${XAUTHORITY:-$HOME/.Xauthority}"
innner_xauthority=/home/demo/.Xauthority

docker run -it --name hl -v /tmp/.X11-unix:/tmp/.X11-unix:ro -v "$XAUTHORITY":$innner_xauthority  -v "/home/acsaba:/home/acsaba" -e DISPLAY -e XAUTHORITY=$innner_xauthority devhaskell
