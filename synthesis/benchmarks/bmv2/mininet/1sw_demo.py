#!/usr/bin/env python2

# Copyright 2013-present Barefoot Networks, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from mininet.net import Mininet
from mininet.topo import Topo
from mininet.log import setLogLevel, info
from mininet.cli import CLI
import subprocess
import re
import plotter
import os

from p4_mininet import P4Switch, P4Host

import argparse
from time import sleep

parser = argparse.ArgumentParser(description='Mininet demo')
parser.add_argument('--behavioral-exe', help='Path to behavioral executable',
                    type=str, action="store", required=True)
parser.add_argument('--thrift-port', help='Thrift server port for table updates',
                    type=int, action="store", default=9090)
parser.add_argument('--num-hosts', help='Number of hosts to connect to switch',
                    type=int, action="store", default=2)
parser.add_argument('--mode', choices=['l2', 'l3'], type=str, default='l3')
parser.add_argument('--json', help='Path to JSON config file',
                    type=str, action="store", required=True)
parser.add_argument('--pcap-dump', help='Dump packets on interfaces to pcap files',
                    type=str, action="store", required=False, default=False)
parser.add_argument('--rules', help='Dump simple_router rules for all paths connectivity',
                    type=str,action="store",required=False)

args = parser.parse_args()


class SingleSwitchTopo(Topo):
    "Single switch connected to n (< 256) hosts."
    def __init__(self, sw_path, json_path, thrift_port, pcap_dump, n, **opts):
        # Initialize topology and default options
        Topo.__init__(self, **opts)

        switch = self.addSwitch('s1',
                                sw_path = sw_path,
                                json_path = json_path,
                                thrift_port = thrift_port,
                                pcap_dump = pcap_dump)
        log_rules = []
        phys_rules = []
        for h in xrange(n):
            hid = h + 1
            ip = "10.0.%d.10" % h
            mac = '00:04:00:00:00:%02x' %h
            host = self.addHost('h%d' % hid,
                                ip = "{}/24".format(ip),
                                mac = mac)
            self.addLink(host, switch)
            log_rules.extend([
                "table_add send_frame rewrite_mac {} => {}".format(str(hid),mac),
                "table_add ipv4_forward set_nhop {0}/32 => {1} {2}".format(ip,mac,str(hid))
                ])
            phys_rules.extend([
                "table_add send_frame rewrite_mac {} => {}".format(str(hid),mac),
                "table_add forward set_dmac {} => {}".format(ip,mac),
                "table_add ipv4_lpm set_nhop {0}/32 => {0} {1}".format(ip,str(hid))
                ])

        if args.rules:
            with open(args.rules,'w') as f:
                for r in log_rules:
                    f.write("%s\n" % r)
            with open(args.rules + "_solution.txt", 'w') as f:
                for r in phys_rules:
                    f.write("%s\n" % r)


def filename(src_idx, tgt_idx):
    return "h{src}_ping_h{tgt}.txt".format(src = str(src_idx + 1), tgt = str(tgt_idx + 1))


def start_ping(net, src_idx, tgt_idx):
    src_name = "h%d" % (src_idx + 1)
    tgt_name = "h%d" % (tgt_idx + 1)
    assert src_name != tgt_name
    hsrc = net.get(src_name)
    htgt = net.get(tgt_name)
    return hsrc.cmd("ping {ip} -c 1 -w 10000 > {fn} & ".format(ip = htgt.IP()
                                                               , fn = filename(src_idx, tgt_idx)))


def run_measurement(net, src_idx, tgt_idx):
    return start_ping(net, src_idx, tgt_idx)


def get_time(f):
    cts = ""
    with open(f,'r') as fp:
        cts = fp.read()

    res = re.findall(r", time (\d+)ms", cts)
    print "trying",f,"got", res
    return res[-1]

def collect_data(num_hosts):
    data = sorted([get_time(filename(src,tgt))
                   for src in xrange(num_hosts)
                   for tgt in xrange(num_hosts)
                   if src != tgt], key=int)
    return data

def process_data(data):
    print "time,num"
    data_dict = {}
    for i,t in enumerate(data):
        print t
        data_dict[int(t)] = 100 * float(i)/float(len(data))
    return data_dict


def plot(data0,data1):
    plotter.plot_series(data0,data1)




def experiment(num_hosts, mode, experiment):
    num_hosts = args.num_hosts
    mode = args.mode

    topo = SingleSwitchTopo(args.behavioral_exe,
                            args.json,
                            args.thrift_port,
                            args.pcap_dump,
                            num_hosts)
    net = Mininet(topo = topo,
                  host = P4Host,
                  switch = P4Switch,
                  controller = None)
    net.start()


    sw_mac = ["00:aa:bb:00:00:%02x" % n for n in xrange(num_hosts)]

    sw_addr = ["10.0.%d.1" % n for n in xrange(num_hosts)]

    for n in xrange(num_hosts):
        h = net.get('h%d' % (n + 1))
        if mode == "l2":
            h.setDefaultRoute("dev eth0")
        else:
            h.setARP(sw_addr[n], sw_mac[n])
            h.setDefaultRoute("dev eth0 via %s" % sw_addr[n])

    for n in xrange(num_hosts):
        h = net.get('h%d' % (n + 1))
        h.describe()

    sleep(1)

    print "Ready !"
    for src in xrange(num_hosts):
        for tgt in xrange(num_hosts):
            if src == tgt :
                continue
            else:
                print run_measurement(net, src, tgt)

    os.system(experiment)

    sleep(4)

    net.stop()

    data = collect_data(num_hosts)
    return process_data(data)


def cleanup():
    os.system("rm h*_ping_h*.txt")


def main():
    cd = "cd /home/ericthewry/research/hybrid/synthesis"
    runtime = "benchmarks/bmv2/simple_router/runtime_CLI.py"
    run_avenir = "./avenir synth benchmarks/bmv2/simple_router_logical.p4 benchmarks/bmv2/simple_router_16.p4 benchmarks/bmv2/no_edits.csv benchmarks/bmv2/no_edits.csv benchmarks/bmv2/fvs -data benchmarks/bmv2/mininet/{0} --thrift -b 100 -e 3 -P4 -I1 benchmarks/real/p4includes/ -I2 benchmarks/real/p4includes/ --no-defaults --min --hints exact --no-deletes --cache-edits".format(args.rules)
    baseline = "cat benchmarks/bmv2/mininet/{0}_solution.txt".format(args.rules)
    experiment_cmd = lambda exp: "{0} && {1} | {2}".format(cd, exp, runtime)
    data0 = experiment(args.num_hosts, args.mode, experiment_cmd(run_avenir))
    cleanup()
    data1 = experiment(args.num_hosts, args.mode, experiment_cmd(baseline))
    cleanup()
    plot(data0, data1)


if __name__ == '__main__':
    setLogLevel( 'info' )
    main()
