
from argparse import ArgumentParser


def make_ip_rule(i):
    assert isinstance(i,int)
    assert i < 512
    snd_byte = 0
    lo_byte = i
    if i > 255  :
        snd_byte = i - 255
        lo_byte = 0
    return "ADD,ipv4,10.0.{0}.{1}/32#32,{2}{2}#48;{2}#9,0".format(snd_byte,lo_byte,i)

def make_ethernet_rule(i):
    assert isinstance(i,int)
    assert i < 512
    return "ADD,ethernet,{0}{0}#48,{0}#9,0".format(i)

def make_punt_rule():
    return "ADD,punt,0&0#16;1#1;0&0#4;0&0#32;0&0#32;0#8,,0"

def main ():
    parser = ArgumentParser(description="Generate n insertions into logical.p4")
    parser.add_argument('num_inserts', metavar="N", type=int,
                        help="The number of insertions to generate")

    args = parser.parse_args()
    max_inserts = (args.num_inserts - 3) / 2

    rules = [make_ip_rule(1),
             make_ethernet_rule(1),
             make_punt_rule()]
    rules.extend([ make_ethernet_rule(i + 2) for i in range(max_inserts)])
    rules.extend([ make_ip_rule(i + 2) for i in range(max_inserts)])

    for r in rules:
        print (r)


if __name__ == "__main__" : main()
