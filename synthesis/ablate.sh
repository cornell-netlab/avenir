# /bin/bash


## No Edit cache
# time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes --domain-restr  --hints --unique-edits | tee ./data/full_of1_4k_NOQ.csv

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes --domain-restr   --hints | tee ./data/full_of1_4k_NOEC_NOUE.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes --domain-restr | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300  --fastcx --cache-queries -w -m --restrict-masks --holes | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES.csv

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES_NOMR.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES_NOMR_NOPATH.csv


## NO PATHS

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks --holes --domain-restr  --hints --unique-edits | tee ./data/full_of1_4k_NOEC_NOPATH.csv

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks --domain-restr  --hints --unique-edits | tee ./data/full_of1_4k_NOEC_NOPATH_NOHOLES.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks --holes --domain-restr   --hints | tee ./data/full_of1_4k_NOEC_NOUE_NOPATH.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks --holes --domain-restr | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NOPATH.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300  --fastcx --cache-queries -w --restrict-masks --holes | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOPATH.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w --restrict-masks | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES_NOPATH.csv

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w  | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES_NOMR_NOPATH.csv



## NO HOLES

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --domain-restr  --hints --unique-edits | tee ./data/full_of1_4k_NOEC_NOHOLES.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --domain-restr   --hints | tee ./data/full_of1_4k_NOEC_NOUE_NOHOLES.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes --domain-restr | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NOPATH.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300  --fastcx --cache-queries -w -m --restrict-masks | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NOHOLES_NODR.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w  --restrict-masks | tee ./data/full_of1_4k_NOEC_NOUE_NOHINTS_NODR_NOHOLES_NOPATH.csv



## No Syntactic help

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes --unique-edits | tee ./data/full_of1_4k_NOEC_NOHINTS_NODR.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks --holes | tee ./data/full_of1_4k_NOEC_NOHINTS_NODR_NOUE.csv


time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m --restrict-masks | tee ./data/full_of1_4k_NOEC_NOHINTS_NODR_NOUE_NOHOLES.csv

time ./motley classbench full 4000 -b 10000 -e 4 -data ./classbench/of1_10000 -s --timeout 300 --fastcx --cache-queries -w -m | tee ./data/full_of1_4k_NOEC_NOHINTS_NODR_NOUE_NOHOLES_NOMR.csv
