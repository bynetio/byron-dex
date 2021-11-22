#!/bin/bash
rm -rf .deps
cabal get mtl aeson text containers bytestring base servant servant-server conferer conferer-dhall warp wai co-log wai-cors optparse-generic -d .deps
hasktags ../../dist-newstyle/src .deps
