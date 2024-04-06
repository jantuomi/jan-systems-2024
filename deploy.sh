#!/bin/bash

set -euxo pipefail

IMAGE=registry.jan.systems/jan-systems-2024:latest

# Build
docker build --platform linux/amd64 -t "$IMAGE" .
docker push "$IMAGE"

# Deploy
ssh jan.systems 'cd deployments/garden && bash dc-restart.sh'
