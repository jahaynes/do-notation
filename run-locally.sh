#!/bin/bash

export CASS_HOSTS="[\"127.0.0.1\"]"
export CASS_PORT=9042

cabal v2-run
