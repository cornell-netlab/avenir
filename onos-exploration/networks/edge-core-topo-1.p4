#!/usr/bin/python

import argparse
from mininet.topo import Topo
from mininet.cli import CLI
from mininet.net import Mininet
from mininet.util import dumpNodeConnections
from mininet.log import setLogLevel
from mininet.node import RemoteController
import os
import sys

ONOS_ROOT_PATH = os.environ['ONOS_ROOT']
sys.path.insert(0, ONOS_ROOT_PATH+'/tools/dev/mininet')
from bmv2 import *

REMOTE_CONTROLLER_IP = '127.0.0.1'


class TriangleNet(Topo):
    # Single switch connected to n hosts
    def __init__(self, **opts):
        # Initialize topology and default optioe
        Topo.__init__(self, **opts)

        if opts is not None:
            self.s1OFVer = opts.get('S1OFVer', '13')
            self.s2OFVer = opts.get('S2OFVer', '13')

        e1 = self.addSwitch('e1', protocols='OpenFlow'+self.s1OFVer)
        e2 = self.addSwitch('e2', protocols='OpenFlow'+self.s2OFVer)
        c1 = self.addSwitch('c1', cls=ONOSBmv2Switch, pipeconf='srcmac-pipeconf')
        c2 = self.addSwitch('c2', cls=ONOSBmv2Switch, pipeconf='srcmac-pipeconf')

        h1 = self.addHost('h1')
        h2 = self.addHost('h2')

        self.addLink(h1, e1)
        self.addLink(h2, e2)

        self.addLink(e1, c1)
        self.addLink(e2, c2)
        self.addLink(c1, c2)

if __name__ == '__main__':
    setLogLevel('info')
    parser = argparse.ArgumentParser()
    parser.add_argument("--S1OFVer", help='OpenFlow version (10,11,...,14) of switch 1 ', default='13')
    parser.add_argument("--S2OFVer", help='OpenFlow version (10,11,...,14) of switch 2 ', default='13')
    args = parser.parse_args()

    print os.environ['ONOS_WEB_USER']
    net = Mininet(topo=TriangleNet(S1OFVer=args.S1OFVer, S2OFVer=args.S2OFVer),
                  controller=None)
                  #controller=RemoteController(name='onos', ip=REMOTE_CONTROLLER_IP, port=6633))
    net.addController("onos", controller=RemoteController, ip=REMOTE_CONTROLLER_IP)
    net.start()
    CLI(net)
net.stop()

