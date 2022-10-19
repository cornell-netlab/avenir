#!/usr/bin/env bash
set -euo pipefail

cd ~/avenir/synthesis/
./avenir server -P4 \
    benchmarks/bmv2/simple_router_logical.p4 benchmarks/bmv2/simple_router_16.p4  -I1 benchmarks/real/p4includes \
    benchmarks/bmv2/no_edits.csv benchmarks/bmv2/no_edits.csv benchmarks/bmv2/fvs -I2 benchmarks/real/p4includes \
    --thrift -b 100 -e 3 \
    --no-defaults --min --hints exact --no-deletes --cache-edits 3 -s -S
cd -
