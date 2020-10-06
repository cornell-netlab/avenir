#!/usr/bin/python3

import argparse
import subprocess
import os
import time
import pexpect



if __name__ == '__main__':
    onos_root = os.environ.get('ONOS_ROOT')
    onos_client = "sh ./onos-client.sh"
    onos_run = "sh ./onos.sh"
    print onos_client
    print onos_run
    
    # onos = pexpect.spawn("gnome-terminal  -- "+onos_run+" --working-directory="+onos_root)
    # time.sleep(5)
   
    onos = subprocess.Popen("gnome-terminal -- "+onos_run+" --working-directory="+onos_root,
                stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    time.sleep(100)
    onos.wait()

    # onos_client = pexpect.spawn("gnome-terminal  -- "+onos_root+"/tools/test/bin/onos localhost")
    # time.sleep(10)
    onos_client = subprocess.Popen("gnome-terminal  -- " +onos_client, 
                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    # onos_client.sendline('app deactivate org.onosproject.fwd')
    # onos_client.stdin.flush()
    # onos_client.wait()





