#!/usr/bin/env bash

# run a command in our Erlang 20.1 container
# cmd args are what would be used locally, e.g.
#
# ./drun ./runtest.sh hooks_SUITE

cmd="docker exec e20 /bin/bash -c 'cd /nts; $1 $2 $3 $4 $5'"
eval $cmd