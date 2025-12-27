#!/bin/bash

set -euxo pipefail

ulimit -n 8192
./update-linklog-json.sh
csi -s main.scm
rsync -rvzP --delete --chown 80:80 out/* $RSYNC_TARGET
