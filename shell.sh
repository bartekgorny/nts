#!/usr/bin/env bash

# run a command in our Erlang 20.1 container
# cmd args are what would be used locally, e.g.
#
# ./drun ./runtest.sh hooks_SUITE

#eval $(docker-machine env)

docker exec -it e20 /bin/bash -c ' cd /nts && rebar3 shell'
