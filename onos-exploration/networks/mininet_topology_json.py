
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


def toJSON(topo):
    print "links "
    links = topo.links(withInfo=True)
    newLinks = []
    hostLinks = []
    for l in links:
        print l
        if topo.isSwitch(l[0]) and topo.isSwitch(l[1]):
            newLinks.append(l[2])
        elif topo.isSwitch(l[0]):
            swport, hport = topo.port(l[0], l[1])
            hostLinks.append({"node":l[0], "port":swport})
        elif topo.isSwitch(l[1]):
            swport, hport = topo.port(l[1], l[0])
            hostLinks.append({"node":l[1], "port":swport})
    topoDict = {"edge":hostLinks, "links":newLinks}
    with open(topo.__class__.__name__ +".json", "w") as write_file:
        json.dump(topoDict, write_file)
    return json.dumps(topoDict)