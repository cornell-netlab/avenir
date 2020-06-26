5# /bin/bash


## Unique Edits is turned off
time ./avenir classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries --cache-edits -w --restrict-masks --holes --domain-restr  --hints | tee ./data/full_of1_4k_NOUE_NOPATHS.csv


## QCache is turned off
time ./avenir classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-edits -w --restrict-masks --holes --domain-restr  --hints | tee ./data/full_of1_4k_NOUE_NOPATHS_NOQCache.csv

## No Edit cache
time ./avenir classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks --holes --domain-restr  --hints | tee ./data/full_of1_4k_NOUE_NOPATHS_NOECache.csv

## No slicing
time ./avenir classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 --timeout 300 --fastcx --cache-queries --cache-edits -w --restrict-masks --holes --domain-restr --hints | tee ./data/full_of1_4k_NOUE_NOPATHS_NOS.csv
