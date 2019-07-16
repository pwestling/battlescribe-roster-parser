#!/bin/bash

set -euo pipefail

stack --docker build
stack --docker install
docker build -t gcr.io/oneoff-project/battlescribe-roster-parser:latest .
docker push gcr.io/oneoff-project/battlescribe-roster-parser:latest
