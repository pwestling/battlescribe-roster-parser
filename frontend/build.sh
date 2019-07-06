#!/bin/bash

set -euo pipefail

elm make src/Main.elm --output html/elm.js
docker build -t gcr.io/oneoff-project/bs2tts-frontend:latest .
docker push gcr.io/oneoff-project/bs2tts-frontend:latest
