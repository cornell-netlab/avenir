#!/usr/bin/env bash

cd $ONOS_ROOT
bazel run onos-local -- clean debug
