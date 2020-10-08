# Hybrid-Network
Currently, this repository contains utility scripts to easily launch ONOS and emulate networks using mininet.

# Contents
1. networks : scripts to emulate various network scenarios
2. onos-launch-scripts : automation scripts to launch ONOS and ONOS client

# Setup
A virtual machine(download from [here](https://drive.google.com/file/d/16c9r8zeo7BJDz2N78XnyikGNBzKUSqXK/view?usp=sharing)) having required packages installed to execute ONOS and network scenarios.
user-name : hybrid-networks
password : hybrid-networks
The VM contains following.
1. ONOS Source : We have forked ONOS [here](https://github.com/cornell-netlab/onos) from its original repository to have a fixed baseline reference for our application development. The ONOS source in the VM is cloned from the forked repository.
This will server two purposes.
    1. A fixed common source code base for everyone
    2. If ONOS from ONF undergoes major changes, we will have option to not include them.
2. Hybrid Source : The VM contains clone of this repository. 
3. Code Browser/Editor : [IntelliJ IDEA](https://www.jetbrains.com/idea/download/#section=linux) to browse the ONOS code.

# How to Start
1. Download the [VM](https://drive.google.com/file/d/16c9r8zeo7BJDz2N78XnyikGNBzKUSqXK/view?usp=sharing)
    Enter Username: `hybrid-networks` and password: `hybrid` to login
2. Use `python hybrid/onos-launch-scripts/launch-onos.py` to execute ONOS.
The script opens two terminals. 1. Runs ONOS in debug more, 2. Runs ONOS client. 
Currently, scripts wait for 100 seconds before launching the client. More automation will be added on need/demand basis. 
3. Use `sudo python hybrid/networks/mn-of-net-1.py` to launch network. It is possible to pass OpenFlow versions as parameters to the script. Use `-h` as a parameter to the script for more details.
