#!/bin/bash

set -euo pipefail

CURRENT=$(git rev-parse --short HEAD)
git add -A
git stash
generate(){
  pushd test_rosters
  git checkout $1
  OUTDIR=$(git rev-parse --short $1)
  mkdir $OUTDIR
  for f in $(ls *.testros) 
  do
    stack exec battlescribe-roster-parser-cli $f 2>/dev/null | tail -n 1 | jq . > $OUTDIR/$f 
  done  
  popd
  git checkout $CURRENT
}

generate $1
generate $2

git stash apply