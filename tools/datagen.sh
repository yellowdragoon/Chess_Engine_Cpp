#!/bin/sh
# Generate self-play training data in parallel, one engine process per worker.
#   tools/datagen.sh <engine> <out dir> <workers> <games per worker> [soft nodes] [base seed]
# Each worker writes <out dir>/<seed>.txt; lines are "fen | white cp | white result".
set -e
ENGINE=$1; OUT=$2; WORKERS=$3; GAMES=$4; NODES=${5:-5000}; SEED=${6:-1000}
mkdir -p "$OUT"
for i in $(seq 1 "$WORKERS"); do
  seed=$((SEED + i))
  "$ENGINE" datagen "$OUT/$seed.txt" "$GAMES" "$NODES" "$seed" 2> "$OUT/$seed.log" &
done
wait
cat "$OUT"/*.txt | wc -l
