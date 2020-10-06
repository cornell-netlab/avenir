#!/usr/bin/python

import argparse
from mininet.topo import Topo

from mininet.cli import CLI
from mininet.net import Mininet
from mininet.util import dumpNodeConnections
from mininet.log import setLogLevel
from mininet.node import RemoteController


REMOTE_CONTROLLER_IP = "127.0.0.1"


class OFNet2S2W(Topo):
    # Single switch connected to n hosts
    def __init__(self, **opts):
        # Initialize topology and default optioe
        Topo.__init__(self, **opts)

        if opts is not None:
            self.s1OFVer = opts.get('S1OFVer', '13')
            self.s2OFVer = opts.get('S2OFVer', '13')

        s1 = self.addSwitch('s1', protocols='OpenFlow'+self.s1OFVer)
        s2 = self.addSwitch('s2', protocols='OpenFlow'+self.s2OFVer)

        h1 = self.addHost('h1')
        h2 = self.addHost('h2')

        self.addLink(h1, s1)
        self.addLink(h2, s2)
        self.addLink(s1, s2)


if __name__ == '__main__':
    setLogLevel('info')
    parser = argparse.ArgumentParser()
    parser.add_argument("--S1OFVer", help='OpenFlow version (10,11,...,14) of switch 1 ', default='13')
    parser.add_argument("--S2OFVer", help='OpenFlow version (10,11,...,14) of switch 2 ', default='13')
    args = parser.parse_args()

    net = Mininet(topo=OFNet2S2W(S1OFVer=args.S1OFVer, S2OFVer=args.S2OFVer),
                  controller=None)
    net.addController("onos",
                      controller=RemoteController,
                      ip=REMOTE_CONTROLLER_IP,
                      port=6633)
    net.start()
    CLI(net)
net.stop()



