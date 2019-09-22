#!/bin/bash

ONE=$(git rev-parse --short $1)
TWO=$(git rev-parse --short $2)

generate(){
  git checkout $1
  stack build
  OUTDIR="test_rosters/$2"
  mkdir -p "$OUTDIR"
  for f in $(ls test_rosters/*.testros) 
  do
    echo "Processing $f"
    stack exec battlescribe-roster-parser-cli $f 2>/dev/null | tail -n 1 | jq . > $OUTDIR/$(basename $f) 
  done  
}

generate $ONE one
generate $TWO two
git checkout master
diff test_rosters/one/ test_rosters/two/
