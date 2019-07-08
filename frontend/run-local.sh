
#!/bin/bash

set -euo pipefail

PORT=${1:-"8080"}

elm make src/Main.elm --output html/elm.js

docker build -t bs2tts-frontend:local .

docker kill bs2tts-frontend-local 2>/dev/null || true
docker rm bs2tts-frontend-local 2>/dev/null || true

docker run -d --name bs2tts-frontend-local -e PORT=$PORT -p $PORT:$PORT bs2tts-frontend:local
