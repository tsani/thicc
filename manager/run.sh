#!/bin/bash

set -e

ETCDCTL_API=3 etcdctl del thicc-state
# I am a bad person.
docker container rm -f $(docker container ls -a -q) >/dev/null 2>&1 || true
exec stack run
