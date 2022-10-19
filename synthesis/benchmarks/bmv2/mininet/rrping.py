from datetime import datetime
from argparse import ArgumentParser
from pythonping import ping


parser = ArgumentParser(description='ping hosts in round-robin fashion,and report first successful ping')
parser.add_argument('hosts', help='ip addresses of hosts',
                    type=str, nargs='+', action="store")
parser.add_argument('-o', help='the output file to store timing data in',
                    type=str, action='store', required=True)
parser.add_argument('-v', help='verbose', action='store_true', default=False)
args = parser.parse_args()


def time_diff_ms(st):
    df = datetime.now() - st
    seconds = df.days * 24 * 60 * 60 + df.seconds
    ms = seconds * 1000 + df.microseconds / 1000.0
    return ms

def to_hostnames(hosts):
    return ["h" + h.split(".")[2]
            for h in hosts]


def main() :
    start = datetime.now()
    unreachable = set(args.hosts) ## hosts who have not yet completed a successful ping test
    print(unreachable)
    reachable = {} ## host id to first timestamp reached
    while unreachable:
        if args.v: print ("unreachable", unreachable)
        if args.v: print ("reachable", reachable)
        for h in unreachable:
            if args.v:
                print ("pinging", h)
            ping_res = ping(h, count=1, timeout=0.01, verbose=False)
            if args.v : print(ping_res)
            if ping_res.success():
                reachable[h] = str(time_diff_ms(start))
                if args.v: print (h, "reachable in", reachable[h])
            else:
                if args.v: print (h, "unreachable")

        unreachable = unreachable - set(reachable.keys())
        if args.v: print("there are", len(unreachable), "elts")
        with open(args.o + ".reachable", 'w') as f:
            f.write('\n'.join(to_hostnames(set(args.hosts) - unreachable)))
        with open(args.o, 'w') as f:
            f.write('\n'.join(reachable.values()))
    exit



if __name__ == "__main__":
    main()
