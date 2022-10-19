#!/bin/sh

# Start this script from inside the terminal (nix-shell) inside the
# VSCode development environment.  The ports are already published to
# the host machine, so the Elm frontend running on the host should
# find the servers.

set -e
server=$(cabal list-bin server)

cabal build
$server -s Table -p 8080 &
$server -s Player -p 8001 -n Mike -i 1 &
$server -s Player -p 8002 -n Peter -i 2 &
$server -s Player -p 8003 -n Nicole -i 3 &
$server -s Player -p 8004 -n Annette -i 4
