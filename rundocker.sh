#!/usr/bin/env bash

# create a docker container which will be used to run commands in erlang 20.1
# set up a docker network to connect to postgres

docker run -d -it --rm --name e20 -v $(pwd):/nts erlang:20.1.2 /bin/bash
docker network create nts_net
docker network connect nts_net e20
docker network connect nts_net nts-postgres

# reminder: if e20 is in nts_net, it can connecto to nts-postgres but not to github
# and the other way round