
#!/bin/bash

set -euo pipefail

PORT=${1:-"8080"}

stack build --no-docker
stack install --no-docker

docker run -d --name parser-redis-local -p6379:6379 redis:latest 2>/dev/null || true
stack --no-docker exec battlescribe-roster-parser
