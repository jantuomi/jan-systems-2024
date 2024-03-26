#!/bin/bash

dir="${SRC_DIR:-pages}"

set -euxo pipefail

csi -s main.scm

while true; do
    fswatch -r -L -1 "${dir}"
    sleep 1
    csi -s main.scm
done
