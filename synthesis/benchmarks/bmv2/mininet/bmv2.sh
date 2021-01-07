#!/usr/bin/env bash
sudo -E python 1sw_demo.py --behavioral-exe $HOME/behavioral-model/targets/simple_router/simple_router --json $HOME/behavioral-model/targets/simple_router/simple_router.json --loc ~/avenir/synthesis --rules rules64 --num-hosts 64
