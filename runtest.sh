#!/bin/bash

rebar3 ct --sys_config test.config

lst=$(ls -rt _build/test/logs | grep ct_run | tail -n 1)
rm lastrun
ln -s "_build/test/logs/$lst" lastrun