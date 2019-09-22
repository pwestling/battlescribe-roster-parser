#!/bin/bash

CURRENT=$(git rev-parse --short HEAD)
generate(){
  git checkout $1
  OUTDIR="test_rosters/$(git rev-parse --short $1)"
  mkdir -p "$OUTDIR"
  for f in $(ls test_rosters/*.testros) 
  do
    echo "Processing $f"
    stack exec battlescribe-roster-parser-cli $f 2>/dev/null | tail -n 1 | jq . > $OUTDIR/$(basename $f) 
  done  
  git checkout $CURRENT
}

generate $1
generate $2