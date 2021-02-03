from argparse import ArgumentParser
import os
import plotter
import csv

parser = ArgumentParser(description='process graphs for microbenchmarks')
parser.add_argument('data', help='the data file',type=str, default=None)
parser.add_argument('output', help='directory for output files', type=str, default=None)

args = parser.parse_args()

def parse_data(f):
    data = []
    with open(f,'r') as csvfile:
        csvrows = csv.DictReader(csvfile)
        for row in csvrows:
            if row["time"] is None or row["time"] == "time" :
                continue
            else:
                row["time"] = float(row["time"]) / 1000.0
                data.append(row)
    return data

def get_bounds(k, data):
    hi = 0
    for d in data:
        if d[k] == k : continue
        if int(d[k]) > hi:
            hi = int(d[k])
    lo = hi
    for d in data:
        if d[k] == k : continue
        if int(d[k]) < lo:
            lo = int(d[k])
    return (lo, hi)

def percent_timeout(xlabel,data, fix=None):
    xs = list(set(int(d[xlabel]) for d in data if d[xlabel] != xlabel))
    timeouts = []
    averages = []
    for x in xs:
        count = 0
        times = []
        total = 0
        for d in data:
            if d[xlabel] == xlabel: continue
            if int(d[xlabel]) != int(x) : continue
            if fix is not None and int(d[fix[0]]) != int(d[fix[0]]): continue
            total += 1
            times.append(min(float(d["time"]), 20.0))
            if d["succeed"] != "true": count += 1
        timeouts.append(float(count)/float(total))
        averages.append(times)
    title_prc = "Percent_Timeout_100Edits_32bits_%s" % xlabel
    title_avg = "Violin_100Edits_32bits_%s" % xlabel
    if fix is not None:
        app_str = str(fix[0]) + str(fix[1])
        title_prc += app_str
        title_avg += app_str
    assert len(xs) == len(averages)
    plotter.scatter(args.output,xs, timeouts, title_prc, xlabel, "% timeout", ylim=(0.0,1.0))
    plotter.violins(args.output,xs, averages, title_avg, xlabel, "time (s)", ylim=(0.0,20.0))



def plot_scatter(x,y,label,data,maxy=None,fix=None):
    xs = []
    ys = []
    for d in data:
        if d[fix[0]] == fix[0]: continue
        if fix is None or int(d[fix[0]]) == int(fix[1]):
            xs.append(int(d[x]))
            if maxy is not None and float(d[y]) > float(maxy):
                ys.append(float(maxy))
            else:
                ys.append(float(d[y]))
    print "data for", fix, len(xs), len(ys)
    if maxy is None:
        ylim = None
    else:
        ylim = (0,20.0)
    plotter.scatter(args.output,xs, ys, label, x, y, ylim=ylim)


def num_bits (data, maxy):
    dataset = {}
    for d in data:
        if d["time"] == "time": continue
        x = (int(d["bits"]) * (int(d["keys"]) + int(d["outs"])))
        time = min(float(d["time"]), maxy)
        if x not in dataset:
             dataset[x] = [time]
        else:
             dataset[x].append(time)
    xs = dataset.keys()
    ys = [dataset[x] for x in xs]
    plotter.violins(args.output, xs, ys, "Bits_100EditsTime", "bits", "time (s)", widths=30)

def main ():
    print "getting data"
    data = parse_data(args.data)

    min_key, max_key = get_bounds ("keys", data)
    min_out, max_outs = get_bounds ("outs", data)

    num_bits(data, 20.0)
    percent_timeout("outs", data)
    percent_timeout("keys", data)
    for i in range(min_key, max_key + 1):
        percent_timeout("outs", data, fix=("keys",i))
        plot_scatter("outs","time", "100Edits_%dKeys"%i, data, maxy=20.0, fix=("keys", i))
    for i in range(min_out, max_outs + 1):
        percent_timeout("keys", data, fix=("outs",i))
        plot_scatter("keys","time", "100Edits_%dOuts"%i, data, maxy=20.0, fix=("outs", i))



if "__main__" == __name__: main()
