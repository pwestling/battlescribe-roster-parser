
#!/bin/bash

set -euo pipefail

PORT=${1:-"8080"}

stack build
stack install

docker network create tts 2>/dev/null || true

docker build -t battlescribe-roster-parser:local .

docker kill roster-parser-local 2>/dev/null || true
docker rm roster-parser-local 2>/dev/null || true

docker run -d --name parser-redis-local --network tts redis:latest 2>/dev/null || true
docker start parser-redis-local 2>/dev/null || true
container=$(docker run --network tts --name roster-parser-local -d -p 8080:$PORT -e REDIS_HOST=parser-redis-local battlescribe-roster-parser:local)
echo "Local container is $container"
docker logs -f $container
