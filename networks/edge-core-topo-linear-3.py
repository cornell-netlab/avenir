#!/usr/bin/python

from __future__ import unicode_literals
import argparse
from mininet.topo import Topo
from mininet.cli import CLI
from mininet.net import Mininet
from mininet.util import dumpNodeConnections
from mininet.log import setLogLevel
from mininet.node import RemoteController
import ipaddress
import os
import sys
import json

ONOS_ROOT_PATH = os.environ['ONOS_ROOT']
sys.path.insert(0, ONOS_ROOT_PATH+'/tools/dev/mininet')
from bmv2 import *

REMOTE_CONTROLLER_IP = '127.0.0.1'


class Linear3(Topo):
    # Single switch connected to n hosts
    def __init__(self, **opts):
        # Initialize topology and default optioe
        Topo.__init__(self, **opts)

        if opts is not None:
            self.edgePipeconf = opts.get('EdgePipeconf', 'sourcerouting-pipeconf-edge')
            self.corePipeconf = opts.get('CorePipeconf', 'sourcerouting-pipeconf-core')

        e1 = self.addSwitch('0', cls=ONOSBmv2Switch, pipeconf=self.edgePipeconf)
        e2 = self.addSwitch('100', cls=ONOSBmv2Switch, pipeconf=self.edgePipeconf)
        c1 = self.addSwitch('50', cls=ONOSBmv2Switch, pipeconf=self.corePipeconf)

        start_ip = int(ipaddress.IPv4Address('10.0.0.1'))

        h1 = self.addHost('h1', ip=str(ipaddress.IPv4Address(start_ip))+'/24', mac='00:00:00:00:00:01')
        start_ip = start_ip + 1
        h2 = self.addHost('h2', ip=str(ipaddress.IPv4Address(start_ip))+'/24', mac='00:00:00:00:00:c8')

        self.addLink(h1, e1, port1=1, port2=10)
        self.addLink(h2, e2, port1=1, port2=10)

        self.addLink(e1, c1, port1=5, port2=15)
        self.addLink(e2, c1, port1=15, port2=5)


if __name__ == '__main__':
    setLogLevel('info')
    parser = argparse.ArgumentParser()
    parser.add_argument("--EdgePipeconf", help='PiPipeconfId of edge p4 devices', default='sourcerouting-pipeconf-edge')
    parser.add_argument("--CorePipeconf", help='PiPipeconfId of core p4 devices', default='sourcerouting-pipeconf-core')
    args = parser.parse_args()

    print os.environ['ONOS_WEB_USER']
    net = Mininet(topo=Linear3(EdgePipeconf=args.EdgePipeconf, CorePipeconf=args.CorePipeconf),
                  controller=None)
                  #controller=RemoteController(name='onos', ip=REMOTE_CONTROLLER_IP, port=6633))
    net.addController("onos", controller=RemoteController, ip=REMOTE_CONTROLLER_IP)

    print "links "
    # print net.topo.links(withInfo=True)
    links = net.topo.links(withInfo=True)
    newLinks = []

    hostLinks = []
    for l in links:
        print l
        if net.topo.isSwitch(l[0]) and net.topo.isSwitch(l[1]):
            newLinks.append(l[2])
        elif net.topo.isSwitch(l[0]):
            swport, hport = net.topo.port(l[0], l[1])
            hostLinks.append({"node":l[0], "port":swport})
        elif net.topo.isSwitch(l[1]):
            swport, hport = net.topo.port(l[1], l[0])
            hostLinks.append({"node":l[1], "port":swport})

    topoDict = {"edge":hostLinks, "links":newLinks}

    with open(net.topo.__class__.__name__ +".json", "w") as write_file:
        json.dump(topoDict, write_file)

    net.start()
    host1 = net.get('h1')
    host2 = net.get('h2')
    r1 = host1.cmd('arp -s 10.0.0.2 00:00:00:00:00:64')
    print r1
    r2 = host2.cmd('arp -s 10.0.0.1 00:00:00:00:00:01')
    print r2
    CLI(net)
net.stop()

