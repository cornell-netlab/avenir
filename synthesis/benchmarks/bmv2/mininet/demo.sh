#! /usr/bin/bash

# start the connectivity graph
python3 ./simulate.py 8 2>&1 > /dev/null &
# start the server
bash ./bmv2-server.sh 2>&1 > /dev/null &
# start mininet
bash ./bmv2.sh
