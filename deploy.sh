#!/bin/bash

set -euxo pipefail
rsync -rvzP --delete --chown 80:80 out/* $RSYNC_TARGET
